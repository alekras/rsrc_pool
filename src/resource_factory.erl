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

%% @since 2013-02-20
%% @copyright 2013-2020 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc
%% The module is a factory of resources. This is a template of behavior that real resource factory 
%% have to follow. 
%%

-module(resource_factory).

%% ====================================================================
%% API functions
%% ====================================================================
-export([behaviour_info/1]).
-export([create/1, destroy/2, validate/2, activate/2, passivate/2]).

%% @private
behaviour_info(callbacks) -> [
    {create, 1},
    {destroy, 2},
    {validate, 2},
    {activate, 2},
    {passivate, 2}
  ];
behaviour_info(_) ->
  undefined.

%% @spec create(_Resource_metadata::term()) -> {ok, Resource}
%% 
%% @doc Creates new resource.
create(_Resource_metadata) ->
 {ok, make_ref()}.

%% @spec destroy(_Resource_metadata::term(), _Resource::term()) -> noreturn()
%% 
%% @doc Destroyes a resource. 
destroy(_Resource_metadata, _Resource) ->
  ok.

%% @spec validate(_Resource_metadata::term(), _Resource::term()) -> boolean()
%% 
%% @doc Validate resource: if resource is alive and valid then returns true, otherwise - false.
validate(_Resource_metadata, _Resource) ->
  true.

%% @spec activate(_Resource_metadata::term(), _Resource::term()) -> ok
%% 
%% @doc Some action during activation of a resource before moving the resource from pool to client (not implemented for default module).
activate(_Resource_metadata, _Resource) ->
  ok.

%% @spec passivate(_Resource_metadata::term(), _Resource::term()) -> ok
%% 
%% @doc Some action during passivation of a resource after returning the resource from use to pool (not implemented for default module).
passivate(_Resource_metadata, _Resource) ->
  ok.
