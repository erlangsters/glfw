%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_monitor_test).
-include_lib("eunit/include/eunit.hrl").

glfw_monitor_test() ->
    [] = glfw:monitors(),
    no_monitor = glfw:primary_monitor(),

    true = glfw:init(),

    Monitors = glfw:monitors(),
    ?assert(erlang:is_list(Monitors)),

    {ok, Monitor} = glfw:primary_monitor(),
    ?assert(erlang:is_reference(Monitor)),

    ok.
