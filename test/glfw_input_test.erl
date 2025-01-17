%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_input_test).
-include_lib("eunit/include/eunit.hrl").

glfw_input_test() ->
    IsSupported = glfw:raw_mouse_motion_supported(),
    ?assert(erlang:is_boolean(IsSupported)),

    ok.