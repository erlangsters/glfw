%%
%% A GLFW demo for testing window state and mutation.
%%
%% It creates a window, prints its state, and uses key bindings to change
%% title, position, size, opacity, iconify/maximize, hide/show, and
%% fullscreen. Close the window or press Escape to exit.
%%
%% It does not call set_window_icon/2 or window_title/1; those are unfinished.
%%
-module(test_glfw_window).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

-record(state, {
    window,
    fullscreen = false,
    windowed_pos = {100, 100},
    windowed_size = {800, 600},
    title_n = 0,
    opacity_n = 0
}).

run() ->
    io:format("GLFW Window - Test Demo~n"),
    glfw:init(),
    test_error_handler:setup(),

    io:format("platform: ~p~n", [glfw:platform()]),
    {ok, Window} = glfw:create_window(800, 600, "GLFW Window - Test Demo"),
    glfw:set_key_handler(Window, self()),

    print_bindings(),
    print_state(Window),

    loop(#state{window = Window}),
    glfw:destroy_window(Window),
    glfw:terminate(),
    ok.

print_bindings() ->
    io:format(
        "Keys (focus the window):~n"
        "  T  set title~n"
        "  P  set position~n"
        "  S  set size~n"
        "  O  cycle opacity~n"
        "  I  iconify    R  restore    M  maximize~n"
        "  H  hide (auto-show after 1.5s)~n"
        "  F  toggle fullscreen on the primary monitor~n"
        "  Escape or close the window to exit~n"
        "~n"
    ).

print_state(Window) ->
    io:format("window state:~n"),
    io:format("  position:          ~p~n", [glfw:window_position(Window)]),
    io:format("  size:              ~p~n", [glfw:window_size(Window)]),
    io:format("  framebuffer size:  ~p~n", [glfw:framebuffer_size(Window)]),
    io:format("  frame size:        ~p~n", [glfw:window_frame_size(Window)]),
    io:format("  content scale:     ~p~n", [glfw:window_content_scale(Window)]),
    io:format("  opacity:           ~p~n", [glfw:window_opacity(Window)]),
    io:format("  iconified:         ~p~n", [glfw:window_attrib(Window, iconified)]),
    io:format("  maximized:         ~p~n", [glfw:window_attrib(Window, maximized)]),
    io:format("  hovered:           ~p~n", [glfw:window_attrib(Window, hovered)]),
    io:format("  visible:           ~p~n", [glfw:window_attrib(Window, visible)]),
    io:format("  resizable:         ~p~n", [glfw:window_attrib(Window, resizable)]),
    io:format("  monitor:           ~p~n", [glfw:window_monitor(Window)]),
    case glfw:window_egl_handle(Window) of
        error ->
            io:format("  egl handle:        error~n");
        Handle ->
            io:format("  egl handle:        ~p~n", [Handle])
    end,
    io:format("~n").

loop(#state{window = Window} = State) ->
    case glfw:window_should_close(Window) of
        true ->
            io:format("Window should close, exiting...~n"),
            ok;
        false ->
            glfw:poll_events(),
            loop(drain_keys(State))
    end.

drain_keys(State) ->
    receive
        #glfw_key{action = press, key = Key} ->
            drain_keys(handle_key(Key, State));
        _Other ->
            drain_keys(State)
    after 0 ->
        State
    end.

handle_key(key_escape, #state{window = Window} = State) ->
    ok = glfw:set_window_should_close(Window, true),
    State;
handle_key(key_t, #state{window = Window, title_n = N} = State) ->
    Title = lists:flatten(io_lib:format("GLFW Window - title ~p", [N])),
    ok = glfw:set_window_title(Window, Title),
    io:format("set title to ~p~n", [Title]),
    print_state(Window),
    State#state{title_n = N + 1};
handle_key(key_p, #state{window = Window} = State) ->
    {X, Y} = glfw:window_position(Window),
    ok = glfw:set_window_position(Window, {X + 40, Y + 40}),
    io:format("set position~n"),
    print_state(Window),
    State;
handle_key(key_s, #state{window = Window} = State) ->
    {Width, Height} = glfw:window_size(Window),
    ok = glfw:set_window_size(Window, {Width + 32, Height + 24}),
    io:format("set size~n"),
    print_state(Window),
    State;
handle_key(key_o, #state{window = Window, opacity_n = N} = State) ->
    Opacity = lists:nth(N rem 3 + 1, [1.0, 0.6, 0.3]),
    ok = glfw:set_window_opacity(Window, Opacity),
    io:format("set opacity to ~p~n", [Opacity]),
    print_state(Window),
    State#state{opacity_n = N + 1};
handle_key(key_i, #state{window = Window} = State) ->
    ok = glfw:iconify_window(Window),
    io:format("iconify~n"),
    print_state(Window),
    State;
handle_key(key_r, #state{window = Window} = State) ->
    ok = glfw:restore_window(Window),
    io:format("restore~n"),
    print_state(Window),
    State;
handle_key(key_m, #state{window = Window} = State) ->
    ok = glfw:maximize_window(Window),
    io:format("maximize~n"),
    print_state(Window),
    State;
handle_key(key_h, #state{window = Window} = State) ->
    ok = glfw:hide_window(Window),
    io:format("hide (showing again in 1.5s)~n"),
    print_state(Window),
    timer:sleep(1500),
    ok = glfw:show_window(Window),
    ok = glfw:focus_window(Window),
    io:format("show~n"),
    print_state(Window),
    State;
handle_key(key_f, State) ->
    toggle_fullscreen(State);
handle_key(_Key, State) ->
    State.

toggle_fullscreen(#state{window = Window, fullscreen = false} = State) ->
    case glfw:primary_monitor() of
        undefined ->
            io:format("no primary monitor; cannot go fullscreen~n"),
            State;
        Monitor ->
            Pos = glfw:window_position(Window),
            Size = glfw:window_size(Window),
            Mode = glfw:video_mode(Monitor),
            ok = glfw:set_window_monitor(
                Window,
                Monitor,
                0,
                0,
                Mode#glfw_video_mode.width,
                Mode#glfw_video_mode.height,
                Mode#glfw_video_mode.refresh_rate
            ),
            io:format("fullscreen on ~p~n", [glfw:monitor_name(Monitor)]),
            print_state(Window),
            State#state{
                fullscreen = true,
                windowed_pos = Pos,
                windowed_size = Size
            }
    end;
toggle_fullscreen(#state{
    window = Window,
    fullscreen = true,
    windowed_pos = {X, Y},
    windowed_size = {Width, Height}
} = State) ->
    ok = glfw:set_window_monitor(Window, undefined, X, Y, Width, Height, dont_care),
    io:format("windowed~n"),
    print_state(Window),
    State#state{fullscreen = false}.
