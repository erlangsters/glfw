%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_cursor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("glfw/include/glfw.hrl").

glfw_cursor_test() ->
    Image = #glfw_image{
        width = 2,
        height = 2,
        pixels = <<
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0
        >>
    },

    error = glfw:create_cursor(Image, {0, 0}),
    error = glfw:create_standard_cursor(arrow),

    true = glfw:init(),

    {ok, Cursor} = glfw:create_cursor(Image, {0, 0}),

    {ok, Cursor1} = glfw:create_standard_cursor(arrow),
    {ok, Cursor2} = glfw:create_standard_cursor(ibeam),
    {ok, Cursor3} = glfw:create_standard_cursor(crosshair),
    {ok, Cursor4} = glfw:create_standard_cursor(pointing_hand),
    {ok, Cursor5} = glfw:create_standard_cursor(resize_ew),
    {ok, Cursor6} = glfw:create_standard_cursor(resize_ns),
    % {ok, Cursor7} = glfw:create_standard_cursor(resize_nwse),
    % {ok, Cursor8} = glfw:create_standard_cursor(resize_nesw),
    % {ok, Cursor9} = glfw:create_standard_cursor(resize_all),
    % {ok, Cursor10} = glfw:create_standard_cursor(not_allowed),

    {ok, Window} = glfw:create_window(640, 480, "Hello world!"),
    glfw:set_cursor(Window, Cursor),

    glfw:set_cursor(Window, default),
    glfw:set_cursor(Window, Cursor1),

    glfw:destroy_window(Window),

    glfw:destroy_cursor(Cursor),

    glfw:destroy_cursor(Cursor1),
    glfw:destroy_cursor(Cursor2),
    glfw:destroy_cursor(Cursor3),
    glfw:destroy_cursor(Cursor4),
    glfw:destroy_cursor(Cursor5),
    glfw:destroy_cursor(Cursor6),
    % glfw:destroy_cursor(Cursor7),
    % glfw:destroy_cursor(Cursor8),
    % glfw:destroy_cursor(Cursor9),
    % glfw:destroy_cursor(Cursor10),

    ok.
