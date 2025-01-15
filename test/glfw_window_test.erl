%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_window_test).
-include_lib("eunit/include/eunit.hrl").

glfw_window_test() ->
    no_window = glfw:create_window(800, 600, "Hello, World!"),

    true = glfw:init(),

    {ok, Window} = glfw:create_window(800, 600, "Hello, World!"),

    false = glfw:window_should_close(Window),
    ok = glfw:set_window_should_close(Window, true),
    true = glfw:window_should_close(Window),
    ok = glfw:set_window_should_close(Window, false),


    timer:sleep(100),

    ok = glfw:destroy_window(Window),

    ok.
