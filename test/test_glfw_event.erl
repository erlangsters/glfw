%%
%% A GLFW demo for testing the event handlers of a window.
%%
%% It simply creates a window, sets up all its event handlers which will then
%% print out the events as they occur.
%%
%% XXX: Print more info about key name/scancode.
%% XXX: Perhaps include framebuffer size handler if it's implemented.
%%
-module(test_glfw_event).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

run() ->
    io:format("GLFW Event - Test Demo~n"),
    glfw:init(),
    test_error_handler:setup(),

    {ok, Window} = glfw:create_window(800, 600, "GLFW Event - Test Demo"),

    Handler = spawn(fun event_handler/0),

    glfw:set_window_position_handler(Window, Handler),
    glfw:set_window_size_handler(Window, Handler),
    glfw:set_window_close_handler(Window, Handler),
    glfw:set_window_refresh_handler(Window, Handler),
    glfw:set_window_focus_handler(Window, Handler),
    glfw:set_window_iconify_handler(Window, Handler),
    glfw:set_window_maximize_handler(Window, Handler),
    glfw:set_window_content_scale_handler(Window, Handler),
    % glfw:set_framebuffer_size_handler(Window, Handler),

    glfw:set_key_handler(Window, Handler),
    glfw:set_char_handler(Window, Handler),
    glfw:set_char_mods_handler(Window, Handler),
    glfw:set_mouse_button_handler(Window, Handler),
    glfw:set_cursor_position_handler(Window, Handler),
    glfw:set_cursor_enter_handler(Window, Handler),
    glfw:set_scroll_handler(Window, Handler),
    glfw:set_drop_handler(Window, Handler),

    io:format("Printing window events... (press Ctrl+C to exit)~n"),
    loop_window(Window),
    glfw:terminate().

loop_window(Window) ->
    case glfw:window_should_close(Window) of
        true ->
            io:format("Window should close, exiting...~n"),
            ok;
        false ->
            glfw:poll_events(),
            timer:sleep(100),
            loop_window(Window)
    end.

event_handler() ->
    receive
        #glfw_window_position{window = _Window, position = {X, Y}} ->
            io:format("window position event (pos: ~p)~n", [{X, Y}]);
        #glfw_window_size{window = _Window, size = {Width, Height}} ->
            io:format("window size event (size: ~p)~n", [{Width, Height}]);
        #glfw_window_close{window = _Window} ->
            io:format("window close event~n");
        #glfw_window_refresh{window = _Window} ->
            io:format("window refresh event~n");
        #glfw_window_focus{window = _Window, focused = Focused} ->
            io:format("window focus event (focused: ~p)~n", [Focused]);
        #glfw_window_iconify{window = _Window, iconified = Iconified} ->
            io:format("window iconify event (iconified: ~p)~n", [Iconified]);
        #glfw_window_maximize{window = _Window, maximized = Maximized} ->
            io:format("window maximize event (maximized: ~p)~n", [Maximized]);
        #glfw_window_content_scale{window = _Window, scale = {XScale, YScale}} ->
            io:format("window content scale event (scale: ~p)~n", [{XScale, YScale}]);
        % #glfw_window_content_scale{} ->
        %     ok;
        #glfw_key{
            window = _Window,
            key = Key,
            scancode = Scancode,
            action = Action,
            mods = Mods
        } ->
            io:format("key event (key: ~p, scancode: ~p, action: ~p, mods: ~p)~n", [Key, Scancode, Action, Mods]);
        #glfw_char{window = _Window, codepoint = Codepoint} ->
            io:format("char event (codepoint: ~p)~n", [Codepoint]);
        #glfw_char_mods{
            window = _Window,
            codepoint = Codepoint,
            mods = Mods
        } ->
            io:format("char mods event (codepoint: ~p, mods: ~p)~n", [Codepoint, Mods]);
        #glfw_mouse_button{window = _Window,
            button = Button,
            action = Action,
            mods = Mods
        } ->
            io:format("mouse button event (button: ~p, action: ~p, mods: ~p)~n", [Button, Action, Mods]);
        #glfw_cursor_position{window = _Window, position = {X, Y}} ->
            io:format("cursor position event (pos: ~p)~n", [{X, Y}]);
        #glfw_cursor_enter{window = _Window, entered = Entered} ->
            io:format("cursor enter event (entered: ~p)~n", [Entered]);
        #glfw_scroll{window = _Window, offset = {XOffset, YOffset}} ->
            io:format("scroll event (offset: ~p)~n", [{XOffset, YOffset}]);
        #glfw_drop{window = _Window, paths = Paths} ->
            io:format("drop event (paths: ~p)~n", [Paths]);
        Event ->
            io:format("unknown event: ~p~n", [Event])
    end,
    event_handler().
