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

    {X1, Y1} = glfw:monitor_position(Monitor),
    ?assert(erlang:is_integer(X1)),
    ?assert(erlang:is_integer(Y1)),

    {X2, Y2, Width2, Height2} = glfw:monitor_work_area(Monitor),
    ?assert(erlang:is_integer(X2)),
    ?assert(erlang:is_integer(Y2)),
    ?assert(erlang:is_integer(Width2)),
    ?assert(erlang:is_integer(Height2)),

    {Width3, Height3} = glfw:monitor_physical_size(Monitor),
    ?assert(erlang:is_integer(Width3)),
    ?assert(erlang:is_integer(Height3)),

    {X4, Y4} = glfw:monitor_content_scale(Monitor),
    ?assert(erlang:is_float(X4)),
    ?assert(erlang:is_float(Y4)),

    ok.
