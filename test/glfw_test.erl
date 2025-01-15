%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_test).
-include_lib("eunit/include/eunit.hrl").

glfw_test() ->
    {Major, Minor, Revision} = glfw:version(),
    ?assert(erlang:is_integer(Major)),
    ?assert(erlang:is_integer(Minor)),
    ?assert(erlang:is_integer(Revision)),

    Version = glfw:version_string(),
    ?assert(erlang:is_list(Version)),

    false = glfw:platform_supported(win32),
    false = glfw:platform_supported(cocoa),
    false = glfw:platform_supported(wayland),
    true = glfw:platform_supported(x11),
    false = glfw:platform_supported(null),

    error = glfw:platform(),
    glfw:init(),
    {ok, x11} = glfw:platform(),

    ok.
