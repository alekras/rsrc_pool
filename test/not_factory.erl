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
%% The module is a factory of resources - factory of mysql connections. 
%%

-module(not_factory).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/1, destroy/2, validate/2, activate/2, passivate/2]).

%% @spec create(Resource_metadata::term()) -> {ok, Resource)
%% 
%% @doc Creates new connection to MySQL server.
create(_) ->
 {ok, make_ref()}.

%% @spec destroy(Resource_metadata::term(), _Resource::pid()) -> noreturn()
%% 
%% @doc Destroyes resource - connection to server. 
destroy(_, _) ->
  ok.

%% @spec validate(Resource_metadata::term(), _Resource::pid()) -> boolean()
%% 
%% @doc Validate connection: if connection is alive returns true, otherwise - false.
validate(_, _) ->
  true.

%% @spec activate(Resource_metadata::term(), _Resource::pid()) -> ok
%% 
%% @doc Some action during activation of connection before moving resource from pool to client - no implementation yet.
activate(_, _) ->
  ok.

%% @spec passivate(Resource_metadata::term(), _Resource::pid()) -> ok
%% 
%% @doc Some action during passivation of connection after returning resource from use to pool - no implementation for awhile.
passivate(_, _) ->
  ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

