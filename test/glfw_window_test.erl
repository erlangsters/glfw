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

    "Hello, World!" = glfw:window_title(Window),
    ok = glfw:set_window_title(Window, "Goodbye, World!"),
    "Goodbye, World!" = glfw:window_title(Window),

    ok = glfw:set_window_icon(Window, "icon.png"),

    {X, Y} = glfw:window_position(Window),
    io:format(user, "window position (x: ~p, y: ~p)~n", [X, Y]),
    ok = glfw:set_window_position(Window, {100, 100}),
    % XXX: Check if the position was actually updated.

    {Width, Height} = glfw:window_size(Window),
    io:format(user, "window size (width: ~p, height: ~p)~n", [Width, Height]),
    ok = glfw:set_window_size(Window, {1024, 768}),
    % XXX: Check if the size was actually updated.

    ok = glfw:set_window_size_limits(Window, {640, 480}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {dont_care, 480}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, dont_care}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {dont_care, dont_care}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, dont_care, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {dont_care, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {1920, dont_care}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {dont_care, dont_care}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, dont_care),

    ok = glfw:set_window_aspect_ratio(Window, {16, 9}),
    ok = glfw:set_window_aspect_ratio(Window, {dont_care, 9}),
    ok = glfw:set_window_aspect_ratio(Window, {16, dont_care}),
    ok = glfw:set_window_aspect_ratio(Window, {dont_care, dont_care}),

    {Left, Top, Right, Bottom} = glfw:window_frame_size(Window),
    io:format(user, "window frame size (left: ~p, top: ~p, right: ~p, bottom: ~p)~n", [Left, Top, Right, Bottom]),

    {XScale, YScale} = glfw:window_content_scale(Window),
    io:format(user, "window content scale (x: ~p, y: ~p)~n", [XScale, YScale]),

    Opacity = glfw:window_opacity(Window),
    io:format(user, "window opacity: ~p~n", [Opacity]),

    ok = glfw:set_window_opacity(Window, 0.5),
    % XXX: Check if the opacity was actually updated.

    ok = glfw:iconify_window(Window),
    ok = glfw:restore_window(Window),
    ok = glfw:maximize_window(Window),
    ok = glfw:show_window(Window),
    ok = glfw:hide_window(Window),
    ok = glfw:focus_window(Window),
    ok = glfw:request_window_attention(Window),

    ok = glfw:destroy_window(Window),

    ok.
