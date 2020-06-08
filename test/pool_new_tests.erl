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

-module(pool_new_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

%%
%% Import modules
%%
-import(resource_pool, []).
-import(resource_pool_tests, [close_pool/1]).

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
      fun do_cleanup/2, 
      [
        {default, fun new_default/2},
        {custom, fun new_custom/2},
        {factory_not_exist, fun new_errors_not_ex/2},
        {not_factory, fun new_errors_not_fact/2},
        {wrong_option, fun new_errors_wrong_opt/2}
      ]
    }
	].

do_setup(X) ->
  ?debug_Fmt("setup: ~p", [X]), 
  {Factory, Options} = set(X),   
  R = case Options of 
    [] -> resource_pool:new(test_pool, Factory, 0);
    _  -> resource_pool:new(test_pool, Factory, 1, Options)
  end,
  case R of
    {ok, Pid} -> Pid;
    {error, _} -> R
  end.

set(default) -> {factory, []};
set(custom) -> {factory, [
    {max_active, 16},
    {max_idle, 12},
    {min_idle, 3},
    {test_on_borrow, true},
    {test_on_return, true},
    {fifo, true},
    {when_exhausted_action, grow},
    {max_wait, 2500}
  ]};
set(factory_not_exist) -> {factory_1, []};
set(not_factory) -> {not_factory, []};
set(wrong_option) -> {factory, [{wrong_option, 0}, {opt, 0}, {max_idle, 4}]};
set(_) -> [].

do_cleanup(_X, R) ->
%%  ?debug_Fmt("cleanup: ~p, ~p",[X, R]),
  case is_pid(R) of
    true -> close_pool(R);
    false -> ok
  end.

new_default(_X, Pool) -> fun() ->
%%  ?debug_Fmt("    : pool new test: ~p",[X]),
%%  ?debug_Fmt("       1. Pool: ~p",[Pool]),
  ?assert(is_pid(Pool)),
  ?assert(is_process_alive(Pool)),
  State = gen_server:call(Pool, get_state),
%%  ?debug_Fmt("       2. State: ~p",[State]),
  ?assertMatch({state, [], [], [], 8, 8, 0, false, false, false, block, infinity, infinity, factory, 0}, State),
  ?PASSED
end.

new_custom(_X, Pool) -> fun() ->
%%  ?debug_Fmt("    : pool new test: ~p",[X]),
%%  ?debug_Fmt("       1. Pool: ~p",[Pool]),
  ?assert(is_pid(Pool)),
  ?assert(is_process_alive(Pool)),
  State = gen_server:call(test_pool, get_state),
%%  ?debug_Fmt("       2. State: ~p",[State]),
  ?assertMatch({state, [], [], [], 16, 12, 3, true, true, true, grow, 2500, infinity, factory, 1}, State),
  ?PASSED
end.

new_errors_not_ex(_X, Pool) -> fun() ->
%%   ?debug_Fmt("    : pool new test: ~p",[X]),
%%   ?debug_Fmt("     1. Pool: ~p",[Pool]),
  ?assertMatch({error, factory_not_exist}, Pool),
  ?PASSED
end.

new_errors_not_fact(_X, Pool) -> fun() ->
%%   ?debug_Fmt("    : pool new test: ~p",[X]),
%%   ?debug_Fmt("       1. Pool: ~p",[Pool]),
  ?assertMatch({error, not_factory}, Pool),
  ?PASSED
end.

new_errors_wrong_opt(_X, Pool) -> fun() ->
%%   ?debug_Fmt("    : pool new test: ~p",[X]),
%%   ?debug_Fmt("       1. Pool: ~p",[Pool]),
  ?assertMatch({error, "Wrong options: wrong_option, opt"}, Pool),
  ?PASSED
end.
