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

%% @since 2013-03-21
%% @copyright 2013-2016 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc
%% Facade for resource pool. 
%%

-module(resource_pool).

%%
%% Include files
%%
-include("settings.hrl").

-define(OPTION_NAMES,[
	max_active, 
	max_idle, 
	min_idle, 
	test_on_borrow, 
	test_on_return, 
	fifo, 
	when_exhausted_action, 
	max_wait, 
	max_idle_time
]).

%% API functions export
-export([
	new/3, 
	new/4, 
	borrow/1, 
	return/2, 
	add/1, 
	invalidate/2, 
	get_num_active/1, 
	get_num_idle/1, 
	get_number/1, 
	clear/1, 
	close/1
]).

%% @spec new(Pool_name::atom(), Factory_module::atom(), Resource_metadata::term()) -> {ok, Resource} | ignore | {error,Error}
%% 
%% @doc Creates and runs new generic server for {@module} with registered name <code>Pool_name</code>. The new resource pool will use
%% Factory_module as a resource factory and Resource_metadata as a metadata to create a new resource.
%%
new(Pool_name, Factory_module, Resource_metadata) ->
  new(Pool_name, Factory_module, Resource_metadata, []).

%% @spec new(Pool_name::atom(), Factory_module::atom(), Resource_metadata::term(), Options::list({Key, Value})) -> {ok, Resource} | ignore | {error,Error}
%% 
%% @doc Creates and runs new generic server for {@module} with registered name <code>Pool_name</code>. The new resource pool will use
%% Factory_module as a resource factory and Resource_metadata as a metadata to create a new resource.
%%<p>The available options are:</p>
%%<dl>
%%  <dt><code>{max_active, integer()}</code></dt>
%%  <dd>defines the maximum number of resource instances that can be allocated by the pool at a given time. 
%%  If non-positive, there is no limit to the number of instances that can be managed by the pool at one time.
%%  When <code>max_active</code> is reached, the pool is said to be exhausted.
%%  The default setting for this parameter is 8.
%%  </dd>
%%
%%  <dt><code>{max_idle, integer()}</code></dt>
%%  <dd>defines the maximum number of objects that can sit idle in the pool at any time.
%%  If negative, there is no limit to the number of objects that may be idle at one time.
%%  The default setting for this parameter equals <code>max_active</code>. 
%%  </dd>
%%
%%  <dt><code>{min_idle, integer()}</code></dt>
%%  <dd>defines the minimum number of "sleeping" instances in the pool. Default value is 0.</dd>
%%
%%  <dt><code>{test_on_borrow, boolean()}</code></dt>
%%  <dd> If true the pool will attempt to validate each resource before it is returned from the borrow function 
%%  (Using the provided resource factory's validate function).
%%  Instances that fail to validate will be dropped from the pool, and a different object will
%%  be borrowed. The default setting for this parameter is <code>false.</code>
%%  </dd>
%%
%%  <dt><code>{test_on_return, boolean()}</code></dt>
%%  <dd> If true the pool will attempt to validate each resource instance before it is returned to the pool in the
%%  return function (Using the provided resource factory's validate function). Objects that fail to validate 
%%  will be dropped from the pool. The default setting for this option is <code>false.</code></dd>
%%
%%  <dt><code>{fifo, boolean()}</code></dt>
%%  <dd> The pool can act as a LIFO queue with respect to idle resource instances 
%%  - always returning the most recently used resource from the pool,
%%  or as a FIFO queue, where borrow always returns the oldest instance from the idle resource list.
%%  <i>fifo</i> determines whether or not the pool returns idle objects in
%%  first-in-first-out order. The default setting for this parameter is <code>false.</code>
%%  </dd>
%%
%%  <dt><code>{when_exhausted_action, (fail | block | grow)}</code></dt>
%%  <dd>specifies the behaviour of the <code>borrow</code> function when the pool is exhausted:
%%  <dl>
%%    <dt>fail</dt><dd>will return an error.</dd>
%%    <dt>block</dt>
%%    <dd>will block until a new or idle object is available. If a positive <i>max_wait</i>
%%    value is supplied, then <code>borrow</code> will block for at most that many milliseconds,
%%    after which an error will be returned. If <i>max_wait</i> is non-positive,
%%    the <code>borrow</code> function will block infinitely.</dd>
%%    <dt>grow</dt>
%%    <dd>will create a new object and return it (essentially making <i>max_active</i> meaningless.)</dd>
%%  </dl>The default <code>when_exhausted_action</code> setting is <code>block</code> and 
%%  the default <code>max_wait</code> setting is <i>infinity</i>. By default, therefore, <code>borrow</code> will
%%  block infinitely until an idle instance becomes available.
%%  </dd>
%%
%%  <dt><code>{max_wait, (integer() | infinity)}</code></dt>
%%  <dd>The maximum amount of time to wait when the <code>borrow</code> function
%%  is invoked, the pool is exhausted (the maximum number
%%  of "active" resource instances has been reached) and <i>when_exhausted_action</i> equals <code>block</code>.
%%  </dd>
%%
%%  <dt><code>{max_idle_time, (integer() | infinity)}</code></dt>
%%  <dd>The maximum amount of time an resource instance may sit idle in the pool,
%%  with the extra condition that at least <i>min_idle</i> amount of object remain in the pool.
%%  When infinity, no instances will be evicted from the pool due to maximum idle time limit.
%%  </dd>
%%</dl>
new(Pool_name, Factory_module, Resource_metadata, Options) ->
%  FailedOptions = lists:filter(fun({Key, _}) -> not lists:member(Key, ?OPTION_NAMES) end, Options),
  FailedOptions = [Key || {Key, _} <- Options, not lists:member(Key, ?OPTION_NAMES)], 
  case FailedOptions of
    [] ->
      case is_factory(Factory_module) of
        true ->
          gen_server:start_link({local, Pool_name}, resource_pool_srv, {Options, Factory_module, Resource_metadata}, [{timeout, ?GEN_SERVER_TIMEOUT}]);
        {error, _} = Er -> Er
      end;
    T -> {error, "Wrong options: " ++ string:join([atom_to_list(K) || K <- T], ", ")}
  end.

%% @spec is_factory(Factory_module) -> boolean()
%% @doc Check Factory_module if it implements resource_factory behaviour.
%% @private 
is_factory(Factory_module) ->
  Module_info = try Factory_module:module_info(attributes) catch error:undef -> [] end,
  case Module_info of
    [] -> {error, factory_not_exist};
    _ ->
      L = proplists:get_value(behaviour, Module_info, []),
      case lists:member(resource_factory, L) of
        true -> true; 
        false -> {error, not_factory}
      end
  end.

%% @spec borrow(Pool_name::atom() | pid()) -> Resource | {error, Reason}
%% 
%% @doc Borrows resource from pool for client use.
borrow(Pool_name) ->
  case gen_server:call(Pool_name, borrow, ?GEN_SERVER_TIMEOUT) of
    {ok, Resource} -> Resource;
    {error, _E} = R -> R;
    {wait, Max_wait} ->
      Receive =
      receive
        {ok, Pid} -> Pid
      after Max_wait -> {error, pool_timeout}
      end,
      gen_server:call(Pool_name, {ack_borrow, Receive}, ?GEN_SERVER_TIMEOUT),
      %% flush message that probably came to the mailbox after timeout
      receive
        {ok, _} -> ok
      after 0 -> ok
      end,
      Receive
  end.

%% @spec return(Pool_name::atom(), Resource::term()) -> ok
%% 
%% @doc Returns resource to pool after client does not need it more.
return(Pool_name, Resource) ->
  gen_server:cast(Pool_name, {return, Resource, self()}).

%% @spec add(Pool_name::atom()) -> ok
%% 
%% @doc Adds one more resource to pool (as an idle resource).
add(Pool_name) ->
  gen_server:cast(Pool_name, add).

%% @spec invalidate(Pool_name::atom(), Resource::term()) -> ok
%% 
%% @doc Invalidates resource - makes it ready to dispose.
invalidate(Pool_name, Resource) ->
  gen_server:call(Pool_name, {invalidate, Resource}, ?GEN_SERVER_TIMEOUT).

%% @spec get_num_active(Pool_name::atom()) -> integer()
%% 
%% @doc Returns number of active (busy) resource in pool.
get_num_active(Pool_name) ->
  gen_server:call(Pool_name, get_num_active, ?GEN_SERVER_TIMEOUT).

%% @spec get_num_idle(Pool_name::atom()) -> integer()
%% 
%% @doc Returns number of idle (ready to use) resource in pool.
get_num_idle(Pool_name) ->
  gen_server:call(Pool_name, get_num_idle, ?GEN_SERVER_TIMEOUT).

%% @spec get_number(Pool_name::atom()) -> integer()
%% 
%% @doc Returns total number of resource in pool.
get_number(Pool_name) ->
  gen_server:call(Pool_name, get_number, ?GEN_SERVER_TIMEOUT).

%% @spec clear(Pool_name::atom()) -> ok
%% 
%% @doc Disposes all resources from the pool.
clear(Pool_name) ->
  gen_server:cast(Pool_name, clear).

%% @spec close(Pool_name::atom()) -> ok
%% 
%% @doc Disposes all resources from the pool and close the pool (shut down generic server). 
close(Pool_name) ->
  gen_server:cast(Pool_name, close).
