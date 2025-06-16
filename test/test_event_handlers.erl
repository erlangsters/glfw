%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(test_event_handlers).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

run() ->
    io:format("aaa~n"),
    true = glfw:init(),
    io:format("bbb~n"),

    {ok, Window} = glfw:create_window(640, 480, "GLFW - Test event handlers"),
    io:format("ccc~n"),

    ok = glfw:set_window_position_handler(Window, self()),
    io:format("ddd~n"),
    % ok = glfw:set_window_size_handler(Window, self()),
    % ok = glfw:set_window_close_handler(Window, self()),
    % ok = glfw:set_window_refresh_handler(Window, self()),
    % ok = glfw:set_window_focus_handler(Window, self()),
    % ok = glfw:set_window_iconify_handler(Window, self()),
    % ok = glfw:set_window_maximize_handler(Window, self()),
    % ok = glfw:set_window_content_scale_handler(Window, self()),

    loop(Window),

    ok = glfw:destroy_window(Window),

    ok.

loop(Window) ->
    glfw:poll_events(),
    process_event(Window, 100),
    case glfw:window_should_close(Window) of
        true ->
            ok;
        false ->
            loop(Window)
    end.

process_event(Window, Timeout) ->
    receive
        #glfw_window_position{window = Window, position = {X, Y}} ->
            io:format("window position event (pos: ~p)~n", [{X, Y}]),
            process_event(Window, Timeout);
        % #glfw_window_size{window = Window, size = {Width, Height}} ->
        %     io:format("window size event (size: ~p)~n", [{Width, Height}]),
        %     process_event(Window, Timeout);
        % #glfw_window_close{window = Window} ->
        %     io:format("window close event~n"),
        %     process_event(Window, Timeout);
        % #glfw_window_refresh{window = Window} ->
        %     io:format("window refresh event~n"),
        %     process_event(Window, Timeout);
        % #glfw_window_focus{window = Window, focused = Focused} ->
        %     io:format("window focus event (focused: ~p)~n", [Focused]),
        %     process_event(Window, Timeout);
        % #glfw_window_iconify{window = Window, iconified = Iconified} ->
        %     io:format("window iconify event (iconified: ~p)~n", [Iconified]),
        %     process_event(Window, Timeout);
        % #glfw_window_maximize{window = Window, maximized = Maximized} ->
        %     io:format("window maximize event (maximized: ~p)~n", [Maximized]),
        %     process_event(Window, Timeout);
        % #glfw_window_content_scale{window = Window, scale = {X, Y}} ->
        %     io:format("window content scale event (scale: ~p)~n", [{X, Y}]),
        %     process_event(Window, Timeout);
        W ->
            io:format("unexpected event: ~p~n", [W])
    after Timeout ->
        ok
    end.
