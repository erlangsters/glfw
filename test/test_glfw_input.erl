%%
%% A GLFW demo for testing input modes, key/mouse queries, clipboard, and drop.
%%
%% It is not a second event printer. Keys in the window change cursor and
%% sticky modes, query current key/mouse state, and read or write the
%% clipboard. Drop files onto the window to print paths.
%%
%% Close the window or press Escape to exit.
%%
-module(test_glfw_input).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

run() ->
    io:format("GLFW Input - Test Demo~n"),
    glfw:init(),
    test_error_handler:setup(),

    {ok, Window} = glfw:create_window(800, 600, "GLFW Input - Test Demo"),
    glfw:set_key_handler(Window, self()),
    glfw:set_drop_handler(Window, self()),

    io:format("raw mouse motion supported: ~p~n", [glfw:raw_mouse_motion_supported()]),
    print_bindings(),
    print_modes(Window),

    loop(Window),
    glfw:destroy_window(Window),
    glfw:terminate(),
    ok.

print_bindings() ->
    io:format(
        "Keys (focus the window):~n"
        "  1  cursor normal     2  hidden     3  disabled     4  captured~n"
        "  K  toggle sticky keys~n"
        "  B  toggle sticky mouse buttons~n"
        "  L  toggle lock key mods~n"
        "  Q  query key_a, key_escape, and mouse_button_1~n"
        "  C  set clipboard string~n"
        "  V  print clipboard string~n"
        "  drop files onto the window to print paths~n"
        "  Escape or close the window to exit~n"
        "~n"
    ).

print_modes(Window) ->
    io:format(
        "input modes: cursor=~p sticky_keys=~p sticky_mouse_buttons=~p "
        "lock_key_mods=~p raw_mouse_motion=~p~n",
        [
            glfw:input_mode(Window, cursor),
            glfw:input_mode(Window, sticky_keys),
            glfw:input_mode(Window, sticky_mouse_buttons),
            glfw:input_mode(Window, lock_key_mods),
            glfw:input_mode(Window, raw_mouse_motion)
        ]
    ).

loop(Window) ->
    case glfw:window_should_close(Window) of
        true ->
            io:format("Window should close, exiting...~n"),
            ok;
        false ->
            glfw:poll_events(),
            drain(Window),
            loop(Window)
    end.

drain(Window) ->
    receive
        #glfw_key{action = press, key = Key} ->
            handle_key(Window, Key),
            drain(Window);
        #glfw_drop{paths = Paths} ->
            io:format("drop paths: ~p~n", [Paths]),
            drain(Window);
        _Other ->
            drain(Window)
    after 0 ->
        ok
    end.

handle_key(Window, key_escape) ->
    ok = glfw:set_window_should_close(Window, true);
handle_key(Window, key_1) ->
    set_cursor(Window, normal);
handle_key(Window, key_2) ->
    set_cursor(Window, hidden);
handle_key(Window, key_3) ->
    set_cursor(Window, disabled);
handle_key(Window, key_4) ->
    set_cursor(Window, captured);
handle_key(Window, key_k) ->
    toggle_bool(Window, sticky_keys);
handle_key(Window, key_b) ->
    toggle_bool(Window, sticky_mouse_buttons);
handle_key(Window, key_l) ->
    toggle_bool(Window, lock_key_mods);
handle_key(Window, key_q) ->
    io:format(
        "query: key_a=~p key_escape=~p mouse_button_1=~p cursor=~p~n",
        [
            glfw:key(Window, key_a),
            glfw:key(Window, key_escape),
            glfw:mouse_button(Window, mouse_button_1),
            glfw:cursor_position(Window)
        ]
    );
handle_key(Window, key_c) ->
    Text = "glfw input demo clipboard",
    ok = glfw:set_clipboard_string(Window, Text),
    io:format("set clipboard to ~p~n", [Text]);
handle_key(Window, key_v) ->
    io:format("clipboard: ~p~n", [glfw:clipboard_string(Window)]);
handle_key(_Window, _Key) ->
    ok.

set_cursor(Window, Mode) ->
    ok = glfw:set_input_mode(Window, cursor, Mode),
    io:format("cursor mode ~p~n", [Mode]),
    print_modes(Window).

toggle_bool(Window, Mode) ->
    New = not glfw:input_mode(Window, Mode),
    ok = glfw:set_input_mode(Window, Mode, New),
    io:format("~p => ~p~n", [Mode, New]),
    print_modes(Window).
