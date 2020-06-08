%%
%% Copyright (C) 2013-2020 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @copyright 2013-2020 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module is running unit tests for helper_common module.

-module(pool_borrow_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

%%
%% Import modules
%%
-import(resource_pool, []).
-import(resource_pool_tests, [f/0, f2/0]).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

resource_pool_test_() ->
	[ 
    {foreachx, 
      fun do_setup/1, 
      fun resource_pool_tests:do_cleanup/2, 
      [
        {min_idle, fun borrow_min_idle/2}, 
        {max_idle_neg, fun borrow_max_idle_neg/2},
        {lifo, fun borrow_lifo/2},
        {fifo, fun borrow_fifo/2},
        {block_test_on_true, fun borrow_block_test_on_true/2},
        {test_on_true, fun borrow_test_on_true/2},
        {block_test_on, fun borrow_block_test_on/2},
        {test_on, fun borrow_test_on/2},
        {max_active_neg, fun borrow_max_active_neg/2},
        {fail_max_active, fun borrow_fail_max_active/2},
        {grow_max_active, fun borrow_grow_max_active/2},
        {block_max_active, fun borrow_block_max_active/2},
        {block_max_wait_timeout, fun borrow_block_max_wait_timeout/2},
        {block_max_wait, fun borrow_block_max_wait/2}
      ]
    }
	].

do_setup(X) ->
  ?debug_Fmt("setup: ~p", [X]), 
  Options = set(X),   
  {ok, Pid} = resource_pool:new(test_pool, factory, 0, Options),
  Pid.

set(min_idle) -> [{max_active, 4}, {min_idle, 2}, {when_exhausted_action, fail}];
set(max_active_neg) -> [{max_active, -1}, {when_exhausted_action, fail}];
set(max_idle_neg) -> [{max_idle, -1}, {when_exhausted_action, grow}];
set(fail_max_active) -> [{max_active, 2}, {when_exhausted_action, fail}];
set(grow_max_active) -> [{max_active, 2}, {when_exhausted_action, grow}];
set(block_max_active) -> [{max_active, 2}, {when_exhausted_action, block}];
set(block_max_wait_timeout) -> [{max_active, 2}, {when_exhausted_action, block}, {max_wait, 2000}];
set(block_max_wait) -> [{max_active, 2}, {when_exhausted_action, block}, {max_wait, 2000}];
set(test_on) -> [];
set(block_test_on) -> [{max_active, 2}, {when_exhausted_action, block}, {max_wait, 200}];
set(test_on_true) -> [{test_on_borrow, true}];
set(block_test_on_true) -> [{max_active, 2}, {test_on_borrow, true}, {when_exhausted_action, block}, {max_wait, 200}];
set(lifo) -> [];
set(fifo) -> [{fifo, true}];
set(_) -> [].

borrow_min_idle(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  resource_pool:borrow(Pool),
%  ?debug_Fmt("       1. pool borrow test: borrow_min_idle() ~p",[f2()]),
  ?assertEqual({1, 2}, f2()),
  resource_pool:borrow(Pool),
%  ?debug_Fmt("       2. pool borrow test: borrow_min_idle() ~p",[f2()]),
  ?assertEqual({2, 2}, f2()),
  resource_pool:borrow(Pool),
%  ?debug_Fmt("       3. pool borrow test: borrow_min_idle() ~p",[f2()]),
  ?assertEqual({3, 2}, f2()),
  resource_pool:borrow(Pool),
%  ?debug_Fmt("       4. pool borrow test: borrow_min_idle() ~p",[f2()]),
  ?assertEqual({4, 2}, f2()),
  Ref = resource_pool:borrow(Pool),
%  ?debug_Fmt("       5. pool borrow test: borrow_min_idle() ~p ~p",[Ref, f2()]),
  ?assertEqual({4, 2}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?assertMatch({error,pool_exhausted}, Ref),
  ?PASSED
end.

borrow_max_active_neg(_X, Pool) -> fun() ->
  [resource_pool:borrow(Pool) || _N <- lists:seq(1, 10)],
  Rsrc_a = resource_pool:borrow(Pool),
  ?assertEqual({11, 0}, f2()),
  ?assert(is_process_alive(Rsrc_a)),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_max_idle_neg(_X, Pool) -> fun() ->
  Resources = [resource_pool:borrow(Pool) || _N <- lists:seq(1, 10)],
  ?assertEqual({10, 0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  [resource_pool:return(Pool, R) || R <- Resources],
  ?assertEqual({0, 10}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  Resources1 = [resource_pool:borrow(Pool) || _N <- lists:seq(1, 10)],
  [resource_pool:add(Pool) || _N <- lists:seq(1, 10)],
  ?assertEqual({10, 10}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  [resource_pool:return(Pool, R) || R <- Resources1],
  ?assertEqual({0, 20}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_fail_max_active(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  Ref = resource_pool:borrow(Pool),
%  ?debug_Fmt("       1. pool borrow test: borrow_max_active() ~p",[Ref]),
  ?assertEqual({2, 0}, f2()),
  ?assertMatch({error, pool_exhausted}, Ref),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_grow_max_active(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  Rsrc_a = resource_pool:borrow(Pool),
%  ?debug_Fmt("     3. pool borrow test: borrow_max_active() ~p",[Rsrc_a]),
  ?assertEqual({3, 0}, f2()),
  ?assert(is_process_alive(Rsrc_a)),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_block_max_active(_X, Pool) -> {timeout, 100, fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  spawn_link(fun() -> 
               Rsrc_a = resource_pool:borrow(Pool),
               timer:sleep(1000), 
               resource_pool:return(Pool, Rsrc_a) 
             end),
  resource_pool:borrow(Pool),
  Rsrc_b = resource_pool:borrow(Pool),
%  ?debug_Fmt("       1. pool borrow test: borrow_max_active() ~p",[Rsrc_b]),
  ?assertEqual({2, 0}, f2()),
  ?assert(is_process_alive(Rsrc_b)),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end}.

borrow_block_max_wait_timeout(_X, Pool) -> {timeout, 100, fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  resource_pool:borrow(Pool),
  resource_pool:borrow(Pool),
  Ref_a = resource_pool:borrow(Pool),
%  ?debug_Fmt("       1. pool borrow test: borrow_block_max_wait() ~p",[Ref_a]),
  ?assertEqual({2, 0}, f2()),
  ?assertMatch({error, pool_timeout}, Ref_a),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end}.

borrow_block_max_wait(_X, Pool) -> {timeout, 100, fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  spawn_link(fun() -> 
               Rsrc_a = resource_pool:borrow(Pool),
               timer:sleep(1000), 
               resource_pool:return(Pool, Rsrc_a)
             end),
  resource_pool:borrow(Pool),
  Rsrc_b = resource_pool:borrow(Pool),
%  ?debug_Fmt("     3. pool borrow test: borrow_block_max_wait() ~p",[Rsrc_b]),
  ?assertEqual({2, 0}, f2()),
  ?assert(is_process_alive(Rsrc_b)),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end}.

borrow_test_on(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  Rsrc = resource_pool:borrow(Pool),
  tst_resource:set_valid(Rsrc, false),
  resource_pool:return(Pool, Rsrc),
  ?assertEqual({0,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  Rsrc1 = resource_pool:borrow(Pool),
  ?assert(is_process_alive(Rsrc1)),
  ?assert(not tst_resource:is_valid(Rsrc1)),
  ?assertEqual({1,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_block_test_on(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  spawn_link(fun() -> 
               Rsrc_a = resource_pool:borrow(Pool),
               tst_resource:set_id(Rsrc_a, 7),
               tst_resource:set_valid(Rsrc_a, false),
               timer:sleep(100), 
               resource_pool:return(Pool, Rsrc_a)
             end),
  resource_pool:borrow(Pool),
  Rsrc_b = resource_pool:borrow(Pool),
%  ?debug_Fmt("     3. pool borrow test: borrow_test_on() ~p",[Rsrc_b]),
  ?assertEqual({2, 0}, f2()),
  ?assert(is_process_alive(Rsrc_b)),
  ?assert(not tst_resource:is_valid(Rsrc_b)),
  ?assertEqual(7, tst_resource:get_id(Rsrc_b)),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_test_on_true(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  Rsrc2 = resource_pool:borrow(Pool),
  tst_resource:set_valid(Rsrc2, false),
  tst_resource:set_id(Rsrc2, 7),
  resource_pool:return(Pool, Rsrc2),
  ?assertEqual({0,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  Rsrc3 = resource_pool:borrow(Pool),
  ?assert(is_process_alive(Rsrc3)),
  ?assert(not is_process_alive(Rsrc2)),
  ?assertNotEqual(7, tst_resource:get_id(Rsrc3)),
  ?assertEqual({1,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_block_test_on_true(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  spawn_link(fun() -> 
               Rsrc_d = resource_pool:borrow(Pool),
               tst_resource:set_id(Rsrc_d, 7),
               tst_resource:set_valid(Rsrc_d, false),
               timer:sleep(100), 
               resource_pool:return(Pool, Rsrc_d)
             end),
  resource_pool:borrow(Pool),
  Rsrc_e = resource_pool:borrow(Pool),
%  ?debug_Fmt("     4. pool borrow test: borrow_test_on() ~p",[Rsrc_e]),
  ?assertEqual({1, 0}, f2()),
  ?assertMatch({error,pool_timeout}, Rsrc_e),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_lifo(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  Rsrc = resource_pool:borrow(Pool),
  ?assert(is_pid(Rsrc)),
  tst_resource:set_id(Rsrc, 0),
  ?assertEqual(1, f()),
  ?assertEqual({1,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc),
  ?assertEqual(1, f()),
  ?assertEqual({0,1}, f2()),
  Rsrc0 = resource_pool:borrow(Pool),
  ?assertEqual(0, tst_resource:get_id(Rsrc0)),
  Rsrc1 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc1, 1),
  Rsrc2 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc2, 2),
  Rsrc3 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc3, 3),
  ?assertEqual(4, f()),
  ?assertEqual({4,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc0),
  ?assertEqual(4, f()),
  ?assertEqual({2,2}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  Rsrc5 = resource_pool:borrow(Pool),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  ?assertEqual(0, tst_resource:get_id(Rsrc5)),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:invalidate(Pool, Rsrc2),
  ?assertEqual(3, f()),
  ?assertEqual({2,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:clear(Pool),
  ?assertEqual(0, f()),
  ?assertEqual({0,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

borrow_fifo(_X, Pool) -> fun() ->
%  ?debug_Fmt("    : pool borrow test: ~p",[X]),
  Rsrc = resource_pool:borrow(Pool),
  ?assert(is_pid(Rsrc)),
  tst_resource:set_id(Rsrc, 0),
  ?assertEqual(1, f()),
  ?assertEqual({1,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc),
  ?assertEqual(1, f()),
  ?assertEqual({0,1}, f2()),
  Rsrc0 = resource_pool:borrow(Pool),
  ?assertEqual(0, tst_resource:get_id(Rsrc0)),
  Rsrc1 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc1, 1),
  Rsrc2 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc2, 2),
  Rsrc3 = resource_pool:borrow(Pool),
  tst_resource:set_id(Rsrc3, 3),
  ?assertEqual(4, f()),
  ?assertEqual({4,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc1),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:return(Pool, Rsrc0),
  ?assertEqual(4, f()),
  ?assertEqual({2,2}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  Rsrc5 = resource_pool:borrow(Pool),
  ?assertEqual(4, f()),
  ?assertEqual({3,1}, f2()),
  ?assertEqual(1, tst_resource:get_id(Rsrc5)),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:invalidate(Pool, Rsrc2),
  ?assertEqual(3, f()),
  ?assertEqual({2,1}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),

  resource_pool:clear(Pool),
  ?assertEqual(0, f()),
  ?assertEqual({0,0}, f2()),
  resource_pool_tests:check_activate_passivate(Pool),
  ?PASSED
end.

%%
%% Local Functions
%%

