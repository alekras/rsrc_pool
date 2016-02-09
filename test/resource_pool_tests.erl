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

%% @hidden
%% @since 2013-02-20
%% @copyright 2013-2016 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module is running unit tests for helper_common module.

-module(resource_pool_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

%%
%% Import modules
%%
-import(resource_pool, []).

%%
%% Exported Functions
%%
-export([close_pool/1, do_cleanup/2, f/0, f2/0, check_activate_passivate/1, worker/3]).

%% redefine inner record for testing only.
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
%% API Functions
%%

resource_pool_test_() ->
	[ 
    {foreachx, 
      fun do_setup/1, 
      fun do_cleanup/2, 
      [
        {pool, fun pool/2},
        {invalidate, fun invalidate/2},
        {clear, fun clear_check/2},
        {stress_test_1, fun stress_test/2},
        {stress_test_2, fun stress_test/2},
        {stress_test_3, fun stress_test/2}
      ]
    }
	].

f() -> resource_pool:get_number(test_pool).

f2() -> {resource_pool:get_num_active(test_pool), resource_pool:get_num_idle(test_pool)}.

check_activate_passivate(Pool) ->
  State = gen_server:call(Pool, get_state),
  [?assert(tst_resource:is_active(Rsrc)) || {Rsrc, _} <- State#state.active],
  [?assertNot(tst_resource:is_active(Rsrc)) || {Rsrc, _} <- State#state.idle].

close_pool(R) ->
  Pid =
  case is_pid(R) of
    true -> R;
    false ->
      case whereis(R) of
        undefined -> ok;
        P -> P
      end
  end,
  case is_pid(Pid) andalso is_process_alive(Pid) of
    true -> timer:sleep(100), resource_pool:close(Pid), close_pool(R);
    false -> ok
  end.

do_setup(X) ->
  ?debug_Fmt("setup: ~p", [X]), 
  Options = set(X),   
  {ok, Pid} = resource_pool:new(test_pool, factory, 0, Options),
  Pid.

set(stress_test_1) -> [{max_active, 20}, {when_exhausted_action, block}, {max_wait, 1}];
set(stress_test_2) -> [{max_active, 20}, {when_exhausted_action, block}, {max_wait, 10}];
set(stress_test_3) -> [{max_active, 10}, {when_exhausted_action, fail}];
set(_) -> [].

do_cleanup(_X, R) ->
%%  ?debug_Fmt("cleanup: ~p, ~p",[_X, R]),
  close_pool(R).

pool(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool common test: ~p",[_X]),
  Rsrc = resource_pool:borrow(Pool),

  ?assertEqual(1, f()),
  ?assertEqual({1, 0}, f2()),

  resource_pool:return(Pool, Rsrc),
  ?assertEqual(1, f()),
  ?assertEqual({0, 1}, f2()),
  Rsrc1 = resource_pool:borrow(Pool),
  Rsrc2 = resource_pool:borrow(Pool),
  Rsrc3 = resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  ?assertEqual(4, f()),
  ?assertEqual({4, 0}, f2()),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3, 1}, f2()),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3, 1}, f2()),

  resource_pool:return(Pool, Rsrc2),
  ?assertEqual(4, f()),
  ?assertEqual({2, 2}, f2()),

  resource_pool:invalidate(Pool, Rsrc3),
  ?assertEqual(3, f()),
  ?assertEqual({1, 2}, f2()),

  resource_pool:clear(Pool),
  ?assertEqual(0, f()),
  ?assertEqual({0, 0}, f2()),
  ?PASSED
end.

invalidate(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool common test: ~p",[_X]),
  resource_pool:borrow(Pool),
  Rsrc = resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  ?assertEqual({4, 0}, f2()),
  Ok = resource_pool:invalidate(Pool, Rsrc),
  ?assertMatch(Ok, ok),
  ?assertEqual({3, 0}, f2()),
  Er = resource_pool:invalidate(Pool, Rsrc),
  ?assertMatch(Er, {error, not_active}),
  ?assertEqual({3, 0}, f2()),
  Self = self(),
  spawn_link(fun() -> 
               Rsrc1 = resource_pool:borrow(Pool), 
               Self ! Rsrc1
             end),
  receive
    Pid ->
      Er1 = resource_pool:invalidate(Pool, Pid),
      ?assertMatch(Er1, {error, not_owner}),
      ?assertEqual({4, 0}, f2()) 
  end,
  ?PASSED
end.

clear_check(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool common test: ~p",[_X]),
  resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  resource_pool:add(Pool),
  resource_pool:add(Pool),
  ?assertEqual({2, 2}, f2()),

  resource_pool:clear(Pool),
  ?assertEqual({0, 0}, f2()),
  ?PASSED
end.

stress_test(_X, Pool) -> {timeout, 1000, fun() ->
  N = 100,
  M = 50,
  run_worker(N, M, Pool),
  wait_for(N),
  State = gen_server:call(Pool, get_state),
  L = State#state.active ++ State#state.idle,
%  ?debug_Fmt("STATE: ~p", [gen_server:call(Pool, get_state)]),
  R = lists:foldl(fun({Rsrc, _}, A) -> A + tst_resource:get_id(Rsrc) end, 0, L),
  ?debug_Fmt("after test ~p", [R]),
  ?assertEqual(N * M, R),
  ?PASSED
end}.
%%
%% Local Functions
%%

run_worker(0, _, _) -> ok;
run_worker(N, M, Pool) ->
%  ?debug_Fmt("run worker ~p", [N]),
  spawn_link(?MODULE, worker, [M, Pool, self()]),
  run_worker(N - 1, M, Pool).

worker(0, _, Parent) -> 
  ?debug_Fmt("~p:: Done", [self()]), 
  Parent ! done;
worker(N, Pool, Parent) ->
  case resource_pool:borrow(Pool) of
    {error, _E} -> 
%      ?debug_Fmt("~p:: after Error ~p N:~p", [self(), _E, N]),
      worker(N, Pool, Parent);
    Resource -> 
%      ?debug_Fmt("~p:: after Borrow resource: ~p", [self(), Resource]),
      Inc = tst_resource:get_id(Resource) + 1,
      Inc = tst_resource:set_id(Resource, Inc),
      resource_pool:return(Pool, Resource),
%      ?debug_Fmt("~p:: after Return resource: ~p", [self(), Resource]),
      worker(N - 1, Pool, Parent)
  end.

wait_for(0) -> ok;
wait_for(N) ->
  receive
    done -> wait_for(N - 1)
  end.
