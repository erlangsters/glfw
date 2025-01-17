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
    true = glfw:init(),

    IsSupported = glfw:raw_mouse_motion_supported(),
    ?assert(erlang:is_boolean(IsSupported)),

    ok = test_keys(),

    {ok, Window} = glfw:create_window(640, 480, "Hello world!"),
    KeyState = glfw:key(Window, key_a),
    ?assert(lists:member(KeyState, [press, release])),
    MouseButtonState = glfw:mouse_button(Window, mouse_button_1),
    ?assert(lists:member(MouseButtonState, [press, release])),

    ok.

test_keys() ->
    Keys = [
        key_space,
        key_apostrophe,
        key_comma,
        key_minus,
        key_period,
        key_slash,
        key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8,
        key_9,
        key_semicolon,
        key_equal,
        key_a, key_b, key_c, key_d, key_e, key_f, key_g, key_h, key_i,
        key_j, key_k, key_l, key_m, key_n, key_o, key_p, key_q, key_r,
        key_s, key_t, key_u, key_v, key_w, key_x, key_y, key_z,
        key_left_bracket,
        key_backslash,
        key_right_bracket,
        key_grave_accent,
        key_world_1,
        key_world_2,
        key_escape,
        key_enter,
        key_tab,
        key_backspace,
        key_insert,
        key_delete,
        key_right,
        key_left,
        key_down,
        key_up,
        key_page_up,
        key_page_down,
        key_home,
        key_end,
        key_caps_lock,
        key_scroll_lock,
        key_num_lock,
        key_print_screen,
        key_pause,
        key_f1, key_f2, key_f3, key_f4, key_f5, key_f6, key_f7, key_f8,
        key_f9, key_f10, key_f11, key_f12, key_f13, key_f14, key_f15,
        key_f16, key_f17, key_f18, key_f19, key_f20, key_f21, key_f22,
        key_f23, key_f24, key_f25,
        key_kp_0, key_kp_1, key_kp_2, key_kp_3, key_kp_4, key_kp_5, key_kp_6,
        key_kp_7, key_kp_8, key_kp_9,
        key_kp_decimal,
        key_kp_divide,
        key_kp_multiply,
        key_kp_subtract,
        key_kp_add,
        key_kp_enter,
        key_kp_equal,
        key_left_shift,
        key_left_control,
        key_left_alt,
        key_left_super,
        key_right_shift,
        key_right_control,
        key_right_alt,
        key_right_super,
        key_menu
    ],

    lists:foreach(fun(Key) ->
        io:format(user, "testing '~p' key...~n", [Key]),
        Scancode = glfw:key_scancode(Key),
        case Scancode of
            undefined ->
                io:format(user, "scancode is undefined~n", []);
            _ ->
                io:format(user, "scancode is ~p~n", [Scancode])
        end,
        KeyName = glfw:key_name({key, Key}),
        io:format(user, "name: ~p (key version)~n", [KeyName]),

        case Scancode of
            undefined ->
                undefined;
            _ ->
                ScancodeName = glfw:key_name({scancode, Scancode}),
                io:format(user, "name: ~p (scancode version)~n", [ScancodeName])
        end
    end, Keys),

    ok.
