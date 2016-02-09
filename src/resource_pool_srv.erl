%%
%% Copyright (C) 2013-2016 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2013-02-20
%% @copyright 2013-2016 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc
%% The module implements resource pool functionality. 
%%

-module(resource_pool_srv).

%%
%% Include files
%%
-include("settings.hrl").

-behaviour(gen_server).
%% behaviour export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% 
%% Behavioural records.
%% 
-record(state, {
  active = [] :: list(),
  idle = [] :: list(),
  waiting = [] :: list(),
  max_active = 8 :: integer(),
  max_idle = 8 :: integer(),
  min_idle = 0 :: integer(),
  test_on_borrow = false :: boolean(),
  test_on_return = false :: boolean(),
  fifo = false :: boolean(),
  when_exhausted_action = block :: fail | grow | block,
  max_wait = infinity :: integer() | infinity,
  max_idle_time = infinity :: integer() | infinity,
  factory_module :: atom(),
  resource_metadata :: term()
}).

%%
%% Behavioural functions and records.
%%

%% @spec init(Args :: term()) -> Result
%% 	Result = {ok, State} | {ok, State, Timeout} | {ok, State, hibernate} | {stop, Reason :: term()} | ignore
%% 	State = term()
%% 	Timeout = non_neg_integer() | infinity
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% 
init({Options, Factory_module, Resource_metadata}) ->
  Max_active = get_option_value(max_active, Options, 8),
  State = #state{
            max_active = Max_active,
            max_idle = get_option_value(max_idle, Options, Max_active),
            min_idle = get_option_value(min_idle, Options, 0),
            max_wait = get_option_value(max_wait, Options, infinity),
            max_idle_time = get_option_value(max_idle_time, Options, infinity),
            when_exhausted_action = get_option_value(when_exhausted_action, Options, block),
            test_on_borrow = get_option_value(test_on_borrow, Options, false),
            test_on_return = get_option_value(test_on_return, Options, false),
            fifo = get_option_value(fifo, Options, false),
            factory_module = Factory_module, 
            resource_metadata = Resource_metadata},
  {ok, State}.

%% @spec get_option_value(Name::atom(), Options::list({Key::atom(), Value::any()}), Default_value::any()) -> Value::any()
%% @private
%% @doc Returns Value for Name key from Options map or Default_value if Options does not contain Name item.
get_option_value(Name, Options, Default_value) ->
  case lists:keyfind(Name, 1, Options) of
    {Name, Value} -> Value;
    false         -> Default_value
  end.

%% @spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result
%% 	Result = {reply, Reply, NewState}
%% 			| {reply, Reply, NewState, Timeout}
%% 			| {reply, Reply, NewState, hibernate}
%% 			| {noreply, NewState}
%% 			| {noreply, NewState, Timeout}
%% 			| {noreply, NewState, hibernate}
%% 			| {stop, Reason, Reply, NewState}
%% 			| {stop, Reason, NewState}
%% 	Reply = term()
%% 	NewState = term()
%% 	Timeout = non_neg_integer() | infinity
%% 	Reason = term()
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% 
handle_call(borrow, From, #state{active = Active, idle = Idle} = State) ->
  Num_active = length(Active),
  Num_idle = length(Idle),
%  io:format(user, " >>> resource_pool_srv:handle_call(borrow, ..) from ~p : ~p  ~p ~p~n", [Owner, Active, Idle, State#state.waiting]),
  process_borrow(From, State, Num_active, Num_idle);
handle_call({ack_borrow, {error, pool_timeout}}, {Waiting_client, _}, #state{active = Active, waiting = Waiting} = State) ->  
%  io:format(user, " >>> resource_pool_srv:handle_call(ack_borrow, ..) from: ~p receive:~p active:~p~n", [Waiting_client, Receive, Active]),
  case lists:keytake({tmp, Waiting_client}, 2, Active) of
    false -> {reply, ok, State#state{waiting = lists:delete(Waiting_client, Waiting)}}; %% pure timeout
    {value, {Resource, _}, New_active} -> {reply, ok, State#state{active = New_active, idle = add_to_idle(Resource, State)}}
  end;
handle_call({ack_borrow, Resource}, {Waiting_client, _}, #state{active = Active} = State) ->  
%  io:format(user, " >>> resource_pool_srv:handle_call(ack_borrow, ..) from: ~p receive:~p active:~p~n", [Waiting_client, Receive, Active]),
  {reply, ok, State#state{active = lists:keyreplace({tmp, Waiting_client}, 2, Active, {Resource, Waiting_client})}};
handle_call(get_all_resources, _From, State) ->
  {reply, lists:map(fun({R, _}) -> R end, State#state.active ++ State#state.idle), State};
handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(get_number, _From, #state{active = Active, idle = Idle} = State) ->
  {reply, length(Active) + length(Idle), State};
handle_call(get_num_active, _From, #state{active = Active} = State) ->
  {reply, length(Active), State};
handle_call(get_num_idle, _From, #state{idle = Idle} = State) ->
  {reply, length(Idle), State};
handle_call({invalidate, Resource}, {Requester, _}, #state{active = Active, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State) ->
  case lists:keyfind(Resource, 1, Active) of
    {Resource, Owner} when Requester =:= Owner -> 
      Factory_mod:destroy(Rsrc_MD, Resource),
      {reply, ok, State#state{active = lists:keydelete(Resource, 1, Active)}};
    {_, _} -> {reply, {error, not_owner}, State};
    false  -> {reply, {error, not_active}, State}
  end.

%% handle_cast/2
%% 
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% 
handle_cast({return, Resource, Requester}, State) ->
  #state{active = Active, idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD, waiting = Waiting} = State,
  Resource_tuple = lists:keyfind(Resource, 1, Active),
%  io:format(user, " >>> resource_pool_srv:handle_cast(return, ..): {~p, ~p} {Rsrc, Owner}:~p Req-er:~p ~p~n", [length(Active), length(Idle), Resource_tuple, Requester, Waiting]),
  case Resource_tuple of
    false -> {noreply, State};
    {Resource, Owner} when Owner =:= Requester ->
      case Waiting of
        [] ->
          New_idle =
          case ((not State#state.test_on_return) orelse (Factory_mod:validate(Rsrc_MD, Resource))) andalso ((State#state.max_idle < 0) orelse (length(Idle) < State#state.max_idle)) of
            true -> add_to_idle(Resource, State);
            false ->
              Factory_mod:destroy(Rsrc_MD, Resource),
              Idle
          end,
          {noreply, State#state{active = lists:keydelete(Resource, 1, Active), idle = New_idle}};
        _ ->
          {Others, [Waiting_client]} = lists:split(length(Waiting) - 1, Waiting),
          case (not State#state.test_on_borrow) orelse Factory_mod:validate(Rsrc_MD, Resource) of
            true ->
%  io:format(user, " --- resource_pool_srv:handle_cast(return, ..): Wait_clt:~p Other_Wait:~p ~n", [Waiting_client, Others]),
              Factory_mod:passivate(Rsrc_MD, Resource),
              Factory_mod:activate({Rsrc_MD, Waiting_client}, Resource),
              Waiting_client ! {ok, Resource},
              {noreply, State#state{active = lists:keyreplace(Resource, 1, Active, {Resource, {tmp, Waiting_client}}), waiting = Others}};
            false ->
              Factory_mod:destroy(Rsrc_MD, Resource),
              {noreply, State#state{active = lists:keydelete(Resource, 1, Active)}}
          end
      end;
    {_, _} -> {noreply, State}
  end;
handle_cast(add, #state{idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State) ->
  case ((State#state.max_idle < 0) orelse (length(Idle) < State#state.max_idle)) of
    true ->
      case Factory_mod:create(Rsrc_MD) of
        {ok, Resource} ->
          {noreply, State#state{idle = add_to_idle(Resource, State)}};
        {error, _Err} ->
          {noreply, State}
      end;
    false -> {noreply, State} 
  end;
handle_cast({remove, {Resource, _}}, #state{idle = Idle, factory_module = Factory_mod} = State) ->
  if
    length(Idle) =< State#state.min_idle -> {noreply, State};
    true                                 ->
      case lists:keytake(Resource, 1, Idle) of
        false ->
          {noreply, State};
        {value, _, New_idle} ->
          Factory_mod:destroy(State#state.resource_metadata, Resource),
          {noreply, State#state{idle = New_idle}}
      end
  end;
handle_cast(clear, #state{active = Active, idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State) ->
  [Factory_mod:destroy(Rsrc_MD, Rsrc) || {Rsrc, _} <- Active],
  [begin Pid ! cancel, Factory_mod:destroy(Rsrc_MD, Rsrc) end || {Rsrc, Pid} <- Idle],
  {noreply, State#state{active = [], idle = []}};
handle_cast(close, #state{active = Active, idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State) ->
  [Factory_mod:destroy(Rsrc_MD, Rsrc) || {Rsrc, _} <- Active],
  [begin Pid ! cancel, Factory_mod:destroy(Rsrc_MD, Rsrc) end || {Rsrc, Pid} <- Idle],
  {stop, normal, State}.

%% handle_info/2
%% ====================================================================
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
  {noreply, State}.

%% terminate/2
%% ====================================================================
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
  ok.

%% code_change/3
%% ====================================================================
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% 
%% Internal functions
%% 

%% @spec add_to_idle(Resource :: atom() | pid(), State::#state{}) -> list({pid(), pid()})
%% @doc
%% @private
add_to_idle(Resource, #state{idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State) ->
  Factory_mod:passivate(Rsrc_MD, Resource),
  Self = self(),
  Pid = spawn_link(fun() -> 
    receive
      cancel -> ok
    after State#state.max_idle_time ->
        gen_server:cast(Self, {remove, {Resource, self()}})
    end
  end),
  [{Resource, Pid} | Idle].

%% @spec process_borrow(From :: tuple(), State::#state{}, Num_active :: integer(), Num_idle :: integer()) -> {reply, Reply, State}
%% @doc
%% @private
process_borrow({Owner, _}, #state{waiting = Waiting, when_exhausted_action = Action} = State, Num_active, _Num_idle) when
    (State#state.max_active > 0) andalso 
    ((Num_active >= State#state.max_active) and (State#state.when_exhausted_action =/= grow)) -> 
  case Action of
    fail -> {reply, {error, pool_exhausted}, State};
    block -> 
      case lists:member(Owner, Waiting) of
        true  -> {reply, {wait, State#state.max_wait}, State};
        false -> {reply, {wait, State#state.max_wait}, State#state{waiting = [Owner | Waiting]}}
      end
  end;
process_borrow({Owner, _} = From, #state{active = Active, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State, _Num_active, Num_idle) when
    Num_idle =< State#state.min_idle ->
  case Factory_mod:create(Rsrc_MD) of
    {ok, Resource} when (Num_idle =:= State#state.min_idle) ->
      Factory_mod:activate({Rsrc_MD, Owner}, Resource),
      {reply, {ok, Resource}, State#state{active = [{Resource, Owner} | Active]}};
    {ok, Resource} ->
      handle_call(borrow, From, State#state{idle = add_to_idle(Resource, State)});
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
process_borrow({Owner, _} = From, #state{active = Active, idle = Idle, factory_module = Factory_mod, resource_metadata = Rsrc_MD} = State, _Num_active, Num_idle) ->
  {Resource, Pid, New_idle} =
  case State#state.fifo of
    false  -> [{Rsrc, P} | N_idle] = Idle, {Rsrc, P, N_idle};
    true -> {N_idle, [{Rsrc, P}]} = lists:split(Num_idle - 1, Idle), {Rsrc, P, N_idle} 
  end,
  Pid ! cancel,
  case (not State#state.test_on_borrow) orelse Factory_mod:validate(Rsrc_MD, Resource) of
    true ->
      Factory_mod:activate({Rsrc_MD, Owner}, Resource),
      {reply, {ok, Resource}, State#state{idle = New_idle, active = [{Resource, Owner} | Active]}};
    false ->
      Factory_mod:destroy(Rsrc_MD, Resource),
      handle_call(borrow, From, State#state{idle = New_idle})
  end.
  