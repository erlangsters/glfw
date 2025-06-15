%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw).
-moduledoc """
GLFW binding.

It provides a binding to the GLFW library, which is used for creating windows
with no OpenGL context and managing input.

```erlang

```

Unlike the C library, GLFW is entirely thread-safe.

> How it works?
> The GLFW library .
> Futhermore, callbacks becomes message. blabla

- when ints are used but they represent enums (implemented with constants), they
re translated to atom

- when ints are used but only GLFW_TRUE and GLFW_FALSE are valid values, they
are translated to boolean()


xxx: talk about memory management

It's a binding, so perhaps the best way to approach it is by looking at how it
differs from the C library. That way, you can assume it behaves the same unless
stated otherwise.

To be written.
""".

-export_type([platform/0]).

-export_type([init_hint_type/0]).
-export_type([init_hint_value/0]).

-export_type([error_code/0]).
-export_type([error_description/0]).

-export_type([monitor/0]).
-export_type([window/0]).
-export_type([cursor/0]).

-export_type([monitor_event/0]).

-export_type([size_limits/0]).
-export_type([aspect_ratio/0]).

-export_type([input_mode/0]).
-export_type([input_mode_value/0]).

-export_type([cursor_shape/0]).

-export_type([key/0]).
-export_type([scancode/0]).

-export_type([mouse_button/0]).

-export_type([joystick/0]).
-export_type([joystick_hat/0]).
-export_type([gamepad_axe/0]).
-export_type([gamepad_button/0]).

-export([init_hint/2]).
-export([init/0]).
-export([terminate/0]).
-export([version/0]).
-export([version_string/0]).
-export([get_error/0]).
-export([error_handler/0]).
-export([set_error_handler/1]).
-export([platform/0]).
-export([platform_supported/1]).

-export([monitors/0]).
-export([primary_monitor/0]).
-export([monitor_position/1]).
-export([monitor_work_area/1]).
-export([monitor_physical_size/1]).
-export([monitor_content_scale/1]).
-export([monitor_name/1]).
-export([monitor_handler/1]).
-export([monitor_set_handler/2]).
-export([monitor_video_modes/1]).
-export([monitor_video_mode/1]).
-export([monitor_set_gamma/2]).
-export([monitor_gamma_ramp/1]).
-export([monitor_set_gamma_ramp/2]).

-export([create_window/3]).
-export([destroy_window/1]).
-export([window_should_close/1]).
-export([set_window_should_close/2]).
-export([window_title/1]).
-export([set_window_title/2]).
-export([set_window_icon/2]).
-export([window_position/1]).
-export([set_window_position/2]).
-export([window_size/1]).
-export([set_window_size/2]).
-export([set_window_size_limits/3]).
-export([set_window_aspect_ratio/2]).
-export([window_frame_size/1]).
-export([window_content_scale/1]).
-export([window_opacity/1]).
-export([set_window_opacity/2]).
-export([iconify_window/1]).
-export([restore_window/1]).
-export([maximize_window/1]).
-export([show_window/1]).
-export([hide_window/1]).
-export([focus_window/1]).
-export([request_window_attention/1]).

-export([window_position_handler/1]).
-export([set_window_position_handler/2]).
-export([window_size_handler/1]).
-export([set_window_size_handler/2]).
-export([window_close_handler/1]).
-export([set_window_close_handler/2]).
-export([window_refresh_handler/1]).
-export([set_window_refresh_handler/2]).
-export([window_focus_handler/1]).
-export([set_window_focus_handler/2]).
-export([window_iconify_handler/1]).
-export([set_window_iconify_handler/2]).
-export([window_maximize_handler/1]).
-export([set_window_maximize_handler/2]).
-export([window_content_scale_handler/1]).
-export([set_window_content_scale_handler/2]).

-export([poll_events/0]).
-export([post_empty_event/0]).

-export([input_mode/2]).
-export([set_input_mode/3]).

-export([create_cursor/2]).
-export([create_standard_cursor/1]).
-export([destroy_cursor/1]).
-export([set_cursor/2]).

-export([raw_mouse_motion_supported/0]).
-export([key_name/1]).
-export([key_scancode/1]).
-export([key/2]).
-export([mouse_button/2]).

-export([cursor_position/1]).
-export([set_cursor_position/2]).

-export([key_handler/1]).
-export([set_key_handler/2]).
-export([char_handler/1]).
-export([set_char_handler/2]).
-export([char_mods_handler/1]).
-export([set_char_mods_handler/2]).
-export([mouse_button_handler/1]).
-export([set_mouse_button_handler/2]).
-export([cursor_position_handler/1]).
-export([set_cursor_position_handler/2]).
-export([cursor_enter_handler/1]).
-export([set_cursor_enter_handler/2]).
-export([scroll_handler/1]).
-export([set_scroll_handler/2]).
-export([drop_handler/1]).
-export([set_drop_handler/2]).

-export([joystick_present/1]).

-export([get_joystick_axes/1]).
-export([get_joystick_buttons/1]).
-export([get_joystick_hats/1]).

-export([get_joystick_guid/1]).

-nifs([init_hint_raw/2]).
-nifs([init/0]).
-nifs([terminate/0]).
-nifs([version/0]).
-nifs([version_string/0]).
-nifs([get_error_raw/0]).
-nifs([error_handler/0]).
-nifs([set_error_handler/1]).
-nifs([platform_raw/0]).
-nifs([platform_supported_raw/1]).

-nifs([monitors/0]).
-nifs([primary_monitor/0]).
-nifs([monitor_position/1]).
-nifs([monitor_work_area/1]).
-nifs([monitor_physical_size/1]).
-nifs([monitor_content_scale/1]).
-nifs([monitor_name/1]).
-nifs([monitor_handler/1]).
-nifs([monitor_set_handler/2]).
-nifs([monitor_video_modes/1]).
-nifs([monitor_video_mode/1]).
-nifs([monitor_set_gamma/2]).
-nifs([monitor_gamma_ramp/1]).
-nifs([monitor_set_gamma_ramp/2]).

-nifs([create_window/3]).
-nifs([destroy_window/1]).
-nifs([window_should_close/1]).
-nifs([set_window_should_close/2]).
-nifs([window_title/1]).
-nifs([set_window_title/2]).
-nifs([set_window_icon/2]).
-nifs([window_position/1]).
-nifs([set_window_position/2]).
-nifs([window_size/1]).
-nifs([set_window_size/2]).
-nifs([set_window_size_limits_raw/5]).
-nifs([set_window_aspect_ratio_raw/3]).
-nifs([window_frame_size/1]).
-nifs([window_content_scale/1]).
-nifs([window_opacity/1]).
-nifs([set_window_opacity/2]).
-nifs([iconify_window/1]).
-nifs([restore_window/1]).
-nifs([maximize_window/1]).
-nifs([show_window/1]).
-nifs([hide_window/1]).
-nifs([focus_window/1]).
-nifs([request_window_attention/1]).

-nifs([window_position_handler/1]).
-nifs([set_window_position_handler/2]).
-nifs([window_size_handler/1]).
-nifs([set_window_size_handler/2]).
-nifs([window_close_handler/1]).
-nifs([set_window_close_handler/2]).
-nifs([window_refresh_handler/1]).
-nifs([set_window_refresh_handler/2]).
-nifs([window_focus_handler/1]).
-nifs([set_window_focus_handler/2]).
-nifs([window_iconify_handler/1]).
-nifs([set_window_iconify_handler/2]).
-nifs([window_maximize_handler/1]).
-nifs([set_window_maximize_handler/2]).
-nifs([window_content_scale_handler/1]).
-nifs([set_window_content_scale_handler/2]).

-nifs([poll_events/0]).
-nifs([post_empty_event/0]).

-nifs([input_mode_raw/2]).
-nifs([set_input_mode_raw/3]).

-nifs([create_cursor_raw/5]).
-nifs([create_standard_cursor_raw/1]).
-nifs([destroy_cursor/1]).
-nifs([set_cursor/2]).

-nifs([raw_mouse_motion_supported/0]).
-nifs([key_name_key/1]).
-nifs([key_name_scancode/1]).
-nifs([key_scancode_raw/1]).
-nifs([key_raw/2]).
-nifs([mouse_button_raw/2]).

-nifs([cursor_position/1]).
-nifs([set_cursor_position_raw/3]).

-nifs([key_handler/1]).
-nifs([set_key_handler/2]).
-nifs([char_handler/1]).
-nifs([set_char_handler/2]).
-nifs([char_mods_handler/1]).
-nifs([set_char_mods_handler/2]).
-nifs([mouse_button_handler/1]).
-nifs([set_mouse_button_handler/2]).
-nifs([cursor_position_handler/1]).
-nifs([set_cursor_position_handler/2]).
-nifs([cursor_enter_handler/1]).
-nifs([set_cursor_enter_handler/2]).
-nifs([scroll_handler/1]).
-nifs([set_scroll_handler/2]).
-nifs([drop_handler/1]).
-nifs([set_drop_handler/2]).

-nifs([joystick_present_raw/1]).

-nifs([get_joystick_axes_raw/1]).
-nifs([get_joystick_buttons_raw/1]).
-nifs([get_joystick_hats_raw/1]).

-nifs([get_joystick_guid_raw/1]).

-export([window_egl_handle/1]).
-nifs([window_egl_handle/1]).

-on_load(init_nif/0).

-define(GLFW_JOYSTICK_HAT_BUTTONS,   16#00050001).
-define(GLFW_ANGLE_PLATFORM_TYPE,    16#00050002).
-define(GLFW_PLATFORM,               16#00050003).
-define(GLFW_COCOA_CHDIR_RESOURCES,  16#00051001).
-define(GLFW_COCOA_MENUBAR,          16#00051002).
-define(GLFW_X11_XCB_VULKAN_SURFACE, 16#00052001).
-define(GLFW_WAYLAND_LIBDECOR,       16#00053001).

-define(GLFW_ANGLE_PLATFORM_TYPE_NONE,     16#00037001).
-define(GLFW_ANGLE_PLATFORM_TYPE_OPENGL,   16#00037002).
-define(GLFW_ANGLE_PLATFORM_TYPE_OPENGLES, 16#00037003).
-define(GLFW_ANGLE_PLATFORM_TYPE_D3D9,     16#00037004).
-define(GLFW_ANGLE_PLATFORM_TYPE_D3D11,    16#00037005).
-define(GLFW_ANGLE_PLATFORM_TYPE_VULKAN,   16#00037007).
-define(GLFW_ANGLE_PLATFORM_TYPE_METAL,    16#00037008).

-define(GLFW_WAYLAND_PREFER_LIBDECOR,  16#00038001).
-define(GLFW_WAYLAND_DISABLE_LIBDECOR, 16#00038002).

-define(GLFW_ANY_PLATFORM,     16#00060000).
-define(GLFW_PLATFORM_WIN32,   16#00060001).
-define(GLFW_PLATFORM_COCOA,   16#00060002).
-define(GLFW_PLATFORM_WAYLAND, 16#00060003).
-define(GLFW_PLATFORM_X11,     16#00060004).
-define(GLFW_PLATFORM_NULL,    16#00060005).

-define(GLFW_FALSE, 0).
-define(GLFW_TRUE, 1).
-define(GLFW_DONT_CARE, -1).

-define(GLFW_CURSOR,               16#00033001).
-define(GLFW_STICKY_KEYS,          16#00033002).
-define(GLFW_STICKY_MOUSE_BUTTONS, 16#00033003).
-define(GLFW_LOCK_KEY_MODS,        16#00033004).
-define(GLFW_RAW_MOUSE_MOTION,     16#00033005).

-define(GLFW_CURSOR_NORMAL,   16#00034001).
-define(GLFW_CURSOR_HIDDEN,   16#00034002).
-define(GLFW_CURSOR_DISABLED, 16#00034003).
-define(GLFW_CURSOR_CAPTURED, 16#00034004).

-define(GLFW_ARROW_CURSOR,         16#00036001).
-define(GLFW_IBEAM_CURSOR,         16#00036002).
-define(GLFW_CROSSHAIR_CURSOR,     16#00036003).
-define(GLFW_POINTING_HAND_CURSOR, 16#00036004).
-define(GLFW_RESIZE_EW_CURSOR,     16#00036005).
-define(GLFW_RESIZE_NS_CURSOR,     16#00036006).
-define(GLFW_RESIZE_NWSE_CURSOR,   16#00036007).
-define(GLFW_RESIZE_NESW_CURSOR,   16#00036008).
-define(GLFW_RESIZE_ALL_CURSOR,    16#00036009).
-define(GLFW_NOT_ALLOWED_CURSOR,   16#0003600A).

-define(GLFW_KEY_SPACE, 32).
-define(GLFW_KEY_APOSTROPHE, 39).
-define(GLFW_KEY_COMMA, 44).
-define(GLFW_KEY_MINUS, 45).
-define(GLFW_KEY_PERIOD, 46).
-define(GLFW_KEY_SLASH, 47).
-define(GLFW_KEY_0, 48).
-define(GLFW_KEY_1, 49).
-define(GLFW_KEY_2, 50).
-define(GLFW_KEY_3, 51).
-define(GLFW_KEY_4, 52).
-define(GLFW_KEY_5, 53).
-define(GLFW_KEY_6, 54).
-define(GLFW_KEY_7, 55).
-define(GLFW_KEY_8, 56).
-define(GLFW_KEY_9, 57).
-define(GLFW_KEY_SEMICOLON, 59).
-define(GLFW_KEY_EQUAL, 61).
-define(GLFW_KEY_A, 65).
-define(GLFW_KEY_B, 66).
-define(GLFW_KEY_C, 67).
-define(GLFW_KEY_D, 68).
-define(GLFW_KEY_E, 69).
-define(GLFW_KEY_F, 70).
-define(GLFW_KEY_G, 71).
-define(GLFW_KEY_H, 72).
-define(GLFW_KEY_I, 73).
-define(GLFW_KEY_J, 74).
-define(GLFW_KEY_K, 75).
-define(GLFW_KEY_L, 76).
-define(GLFW_KEY_M, 77).
-define(GLFW_KEY_N, 78).
-define(GLFW_KEY_O, 79).
-define(GLFW_KEY_P, 80).
-define(GLFW_KEY_Q, 81).
-define(GLFW_KEY_R, 82).
-define(GLFW_KEY_S, 83).
-define(GLFW_KEY_T, 84).
-define(GLFW_KEY_U, 85).
-define(GLFW_KEY_V, 86).
-define(GLFW_KEY_W, 87).
-define(GLFW_KEY_X, 88).
-define(GLFW_KEY_Y, 89).
-define(GLFW_KEY_Z, 90).
-define(GLFW_KEY_LEFT_BRACKET, 91).
-define(GLFW_KEY_BACKSLASH, 92).
-define(GLFW_KEY_RIGHT_BRACKET, 93).
-define(GLFW_KEY_GRAVE_ACCENT, 96).
-define(GLFW_KEY_WORLD_1, 161).
-define(GLFW_KEY_WORLD_2, 162).
-define(GLFW_KEY_ESCAPE, 256).
-define(GLFW_KEY_ENTER, 257).
-define(GLFW_KEY_TAB, 258).
-define(GLFW_KEY_BACKSPACE, 259).
-define(GLFW_KEY_INSERT, 260).
-define(GLFW_KEY_DELETE, 261).
-define(GLFW_KEY_RIGHT, 262).
-define(GLFW_KEY_LEFT, 263).
-define(GLFW_KEY_DOWN, 264).
-define(GLFW_KEY_UP, 265).
-define(GLFW_KEY_PAGE_UP, 266).
-define(GLFW_KEY_PAGE_DOWN, 267).
-define(GLFW_KEY_HOME, 268).
-define(GLFW_KEY_END, 269).
-define(GLFW_KEY_CAPS_LOCK, 280).
-define(GLFW_KEY_SCROLL_LOCK, 281).
-define(GLFW_KEY_NUM_LOCK, 282).
-define(GLFW_KEY_PRINT_SCREEN, 283).
-define(GLFW_KEY_PAUSE, 284).
-define(GLFW_KEY_F1, 290).
-define(GLFW_KEY_F2, 291).
-define(GLFW_KEY_F3, 292).
-define(GLFW_KEY_F4, 293).
-define(GLFW_KEY_F5, 294).
-define(GLFW_KEY_F6, 295).
-define(GLFW_KEY_F7, 296).
-define(GLFW_KEY_F8, 297).
-define(GLFW_KEY_F9, 298).
-define(GLFW_KEY_F10, 299).
-define(GLFW_KEY_F11, 300).
-define(GLFW_KEY_F12, 301).
-define(GLFW_KEY_F13, 302).
-define(GLFW_KEY_F14, 303).
-define(GLFW_KEY_F15, 304).
-define(GLFW_KEY_F16, 305).
-define(GLFW_KEY_F17, 306).
-define(GLFW_KEY_F18, 307).
-define(GLFW_KEY_F19, 308).
-define(GLFW_KEY_F20, 309).
-define(GLFW_KEY_F21, 310).
-define(GLFW_KEY_F22, 311).
-define(GLFW_KEY_F23, 312).
-define(GLFW_KEY_F24, 313).
-define(GLFW_KEY_F25, 314).
-define(GLFW_KEY_KP_0, 320).
-define(GLFW_KEY_KP_1, 321).
-define(GLFW_KEY_KP_2, 322).
-define(GLFW_KEY_KP_3, 323).
-define(GLFW_KEY_KP_4, 324).
-define(GLFW_KEY_KP_5, 325).
-define(GLFW_KEY_KP_6, 326).
-define(GLFW_KEY_KP_7, 327).
-define(GLFW_KEY_KP_8, 328).
-define(GLFW_KEY_KP_9, 329).
-define(GLFW_KEY_KP_DECIMAL, 330).
-define(GLFW_KEY_KP_DIVIDE, 331).
-define(GLFW_KEY_KP_MULTIPLY, 332).
-define(GLFW_KEY_KP_SUBTRACT, 333).
-define(GLFW_KEY_KP_ADD, 334).
-define(GLFW_KEY_KP_ENTER, 335).
-define(GLFW_KEY_KP_EQUAL, 336).
-define(GLFW_KEY_LEFT_SHIFT, 340).
-define(GLFW_KEY_LEFT_CONTROL, 341).
-define(GLFW_KEY_LEFT_ALT, 342).
-define(GLFW_KEY_LEFT_SUPER, 343).
-define(GLFW_KEY_RIGHT_SHIFT, 344).
-define(GLFW_KEY_RIGHT_CONTROL, 345).
-define(GLFW_KEY_RIGHT_ALT, 346).
-define(GLFW_KEY_RIGHT_SUPER, 347).
-define(GLFW_KEY_MENU, 348).

-define(GLFW_MOD_SHIFT, 16#0001).
-define(GLFW_MOD_CONTROL, 16#0002).
-define(GLFW_MOD_ALT, 16#0004).
-define(GLFW_MOD_SUPER, 16#0008).
-define(GLFW_MOD_CAPS_LOCK, 16#0010).
-define(GLFW_MOD_NUM_LOCK, 16#0020).

-define(GLFW_MOUSE_BUTTON_1, 0).
-define(GLFW_MOUSE_BUTTON_2, 1).
-define(GLFW_MOUSE_BUTTON_3, 2).
-define(GLFW_MOUSE_BUTTON_4, 3).
-define(GLFW_MOUSE_BUTTON_5, 4).
-define(GLFW_MOUSE_BUTTON_6, 5).
-define(GLFW_MOUSE_BUTTON_7, 6).
-define(GLFW_MOUSE_BUTTON_8, 7).
-define(GLFW_MOUSE_BUTTON_LEFT,   ?GLFW_MOUSE_BUTTON_1).
-define(GLFW_MOUSE_BUTTON_RIGHT,  ?GLFW_MOUSE_BUTTON_2).
-define(GLFW_MOUSE_BUTTON_MIDDLE, ?GLFW_MOUSE_BUTTON_3).

-define(GLFW_JOYSTICK_1,  0).
-define(GLFW_JOYSTICK_2,  1).
-define(GLFW_JOYSTICK_3,  2).
-define(GLFW_JOYSTICK_4,  3).
-define(GLFW_JOYSTICK_5,  4).
-define(GLFW_JOYSTICK_6,  5).
-define(GLFW_JOYSTICK_7,  6).
-define(GLFW_JOYSTICK_8,  7).
-define(GLFW_JOYSTICK_9,  8).
-define(GLFW_JOYSTICK_10, 9).
-define(GLFW_JOYSTICK_11, 10).
-define(GLFW_JOYSTICK_12, 11).
-define(GLFW_JOYSTICK_13, 12).
-define(GLFW_JOYSTICK_14, 13).
-define(GLFW_JOYSTICK_15, 14).
-define(GLFW_JOYSTICK_16, 15).

-define(GLFW_HAT_CENTERED, 0).
-define(GLFW_HAT_UP, 1).
-define(GLFW_HAT_RIGHT, 2).
-define(GLFW_HAT_DOWN, 4).
-define(GLFW_HAT_LEFT, 8).
-define(GLFW_HAT_RIGHT_UP, (?GLFW_HAT_RIGHT bor ?GLFW_HAT_UP)).
-define(GLFW_HAT_RIGHT_DOWN, (?GLFW_HAT_RIGHT bor ?GLFW_HAT_DOWN)).
-define(GLFW_HAT_LEFT_UP, (?GLFW_HAT_LEFT bor ?GLFW_HAT_UP)).
-define(GLFW_HAT_LEFT_DOWN, (?GLFW_HAT_LEFT bor ?GLFW_HAT_DOWN)).

-define(GLFW_GAMEPAD_AXIS_LEFT_X, 0).
-define(GLFW_GAMEPAD_AXIS_LEFT_Y, 1).
-define(GLFW_GAMEPAD_AXIS_RIGHT_X, 2).
-define(GLFW_GAMEPAD_AXIS_RIGHT_Y, 3).
-define(GLFW_GAMEPAD_AXIS_LEFT_TRIGGER, 4).
-define(GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER, 5).

-define(GLFW_GAMEPAD_BUTTON_A, 0).
-define(GLFW_GAMEPAD_BUTTON_B, 1).
-define(GLFW_GAMEPAD_BUTTON_X, 2).
-define(GLFW_GAMEPAD_BUTTON_Y, 3).
-define(GLFW_GAMEPAD_BUTTON_LEFT_BUMPER, 4).
-define(GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER, 5).
-define(GLFW_GAMEPAD_BUTTON_BACK, 6).
-define(GLFW_GAMEPAD_BUTTON_START, 7).
-define(GLFW_GAMEPAD_BUTTON_GUIDE, 8).
-define(GLFW_GAMEPAD_BUTTON_LEFT_THUMB, 9).
-define(GLFW_GAMEPAD_BUTTON_RIGHT_THUMB, 10).
-define(GLFW_GAMEPAD_BUTTON_DPAD_UP, 11).
-define(GLFW_GAMEPAD_BUTTON_DPAD_RIGHT, 12).
-define(GLFW_GAMEPAD_BUTTON_DPAD_DOWN, 13).
-define(GLFW_GAMEPAD_BUTTON_DPAD_LEFT, 14).
-define(GLFW_GAMEPAD_BUTTON_CROSS, ?GLFW_GAMEPAD_BUTTON_A).
-define(GLFW_GAMEPAD_BUTTON_CIRCLE, ?GLFW_GAMEPAD_BUTTON_B).
-define(GLFW_GAMEPAD_BUTTON_SQUARE, ?GLFW_GAMEPAD_BUTTON_X).
-define(GLFW_GAMEPAD_BUTTON_TRIANGLE, ?GLFW_GAMEPAD_BUTTON_Y).

-define(GLFW_NO_ERROR,              0).
-define(GLFW_NOT_INITIALIZED,       16#00010001).
-define(GLFW_NO_CURRENT_CONTEXT,    16#00010002).
-define(GLFW_INVALID_ENUM,          16#00010003).
-define(GLFW_INVALID_VALUE,         16#00010004).
-define(GLFW_OUT_OF_MEMORY,         16#00010005).
-define(GLFW_API_UNAVAILABLE,       16#00010006).
-define(GLFW_VERSION_UNAVAILABLE,   16#00010007).
-define(GLFW_PLATFORM_ERROR,        16#00010008).
-define(GLFW_FORMAT_UNAVAILABLE,    16#00010009).
-define(GLFW_NO_WINDOW_CONTEXT,     16#0001000A).
-define(GLFW_CURSOR_UNAVAILABLE,    16#0001000B).
-define(GLFW_FEATURE_UNAVAILABLE,   16#0001000C).
-define(GLFW_FEATURE_UNIMPLEMENTED, 16#0001000D).
-define(GLFW_PLATFORM_UNAVAILABLE,  16#0001000E).

-type platform() :: win32 | cocoa | wayland | x11 | null.

-doc "The valid values of the `joystick_hat_buttons` init hint.".
-type joystick_hat_buttons_hint_value() :: boolean().
-doc "The valid values of the `platform` hint.".
-type platform_hint_value() :: any | platform().

-doc "The valid values of the `angle_platform_type` init hint.".
-type angle_platform_hint_value() ::
    angle_platform_type_none |
    angle_platform_type_opengl |
    angle_platform_type_opengles |
    angle_platform_type_d3d9 |
    angle_platform_type_d3d11 |
    angle_platform_type_vulkan |
    angle_platform_type_metal
.
-doc "The valid values of the `cocoa_chdir_resources` init hint.".
-type cocoa_chdir_resources_hint_value() :: boolean().
-doc "The valid values of the `cocoa_chdir_resources` init hint.".
-type cocoa_menubar_hint_value() :: boolean().
-doc "The valid values of the `wayland_libdecor` init hint.".
-type way_libdecor_hint_value() ::
    wayland_prefer_libdecor |
    wayland_disable_libdecor
.

-doc """
The available init hints.

| Hint                    | Note                               | Values                               |
|-------------------------|------------------------------------|--------------------------------------|
| `platform`              | Platform selection init hint.      | `platform_hint_value()`              |
| `joystick_hat_buttons`  | Joystick hat buttons init hint.    | `joystick_hat_buttons_hint_value()`  |
| `angle_platform_type`   | ANGLE rendering backend init hint. | `angle_platform_hint_value()`        |
| `cocoa_chdir_resources` | macOS specific init hint.          | `cocoa_chdir_resources_hint_value()` |
| `cocoa_menubar`         | macOS specific init hint.          | `cocoa_menubar_hint_value()`         |
| `wayland_libdecor`      | Wayland specific init hint.        | `way_libdecor_hint_value()`          |
""".
-type init_hint_type() ::
    platform |
    joystick_hat_buttons |
    angle_platform_type |
    cocoa_chdir_resources |
    cocoa_menubar |
    wayland_libdecor
.
-doc """
The valid values for each init hint.
""".
-type init_hint_value() ::
    platform_hint_value() |
    joystick_hat_buttons_hint_value() |
    angle_platform_hint_value() |
    cocoa_chdir_resources_hint_value() |
    cocoa_menubar_hint_value() |
    way_libdecor_hint_value()
.

-type error_code() ::
    not_initialized |
    no_current_context |
    invalid_enum |
    invalid_value |
    out_of_memory |
    api_unavailable |
    version_unavailable |
    platform_error |
    format_unavailable |
    no_window_context |
    cursor_unavailable |
    feature_unavailable |
    feature_unimplemented |
    platform_unavailable
.
-type error_description() :: undefined | string().

-type monitor() :: reference().
-type window() :: reference().
-type cursor() :: reference().

-type monitor_event() :: connected | disconnected.

-type size_limits() :: {
    Width :: integer() | dont_care,
    Height :: integer() | dont_care
} | dont_care.
-type aspect_ratio() :: {
    Numerator :: integer() | dont_care,
    Denominator :: integer() | dont_care
} | dont_care.

-doc "To be written.".
-type input_mode() ::
    cursor |
    sticky_keys |
    sticky_mouse_buttons |
    lock_key_mods |
    raw_mouse_motion
.

-type cursor_mode() :: normal | hidden | disabled | captured.
-type sticky_keys() :: boolean().
-type sticky_mouse_buttons() :: boolean().
-type lock_key_mods() :: boolean().
-type raw_mouse_motion() :: boolean().

-doc "To be written.".
-type input_mode_value() ::
    cursor_mode() |
    sticky_keys() |
    sticky_mouse_buttons() |
    lock_key_mods() |
    raw_mouse_motion()
.

-doc "Standard system cursor shapes.".
-type cursor_shape() ::
    arrow |
    ibeam |
    crosshair |
    pointing_hand |
    resize_ew |
    resize_ns |
    resize_nwse |
    resize_nesw |
    resize_all |
    not_allowed
.

-doc "Keyboard key tokens.".
-type key() ::
    key_space |
    key_apostrophe |
    key_comma |
    key_minus |
    key_period |
    key_slash |
    key_0 | key_1 | key_2 | key_3 | key_4 | key_5 | key_6 | key_7 | key_8 |
    key_9 |
    key_semicolon |
    key_equal |
    key_a | key_b | key_c | key_d | key_e | key_f | key_g | key_h | key_i |
    key_j | key_k | key_l | key_m | key_n | key_o | key_p | key_q | key_r |
    key_s | key_t | key_u | key_v | key_w | key_x | key_y | key_z |
    key_left_bracket |
    key_backslash |
    key_right_bracket |
    key_grave_accent |
    key_world_1 |
    key_world_2 |
    key_escape |
    key_enter |
    key_tab |
    key_backspace |
    key_insert |
    key_delete |
    key_right |
    key_left |
    key_down |
    key_up |
    key_page_up |
    key_page_down |
    key_home |
    key_end |
    key_caps_lock |
    key_scroll_lock |
    key_num_lock |
    key_print_screen |
    key_pause |
    key_f1 | key_f2 | key_f3 | key_f4 | key_f5 | key_f6 | key_f7 | key_f8 |
    key_f9 | key_f10 | key_f11 | key_f12 | key_f13 | key_f14 | key_f15 |
    key_f16 | key_f17 | key_f18 | key_f19 | key_f20 | key_f21 | key_f22 |
    key_f23 | key_f24 | key_f25 |
    key_kp_0 | key_kp_1 | key_kp_2 | key_kp_3 | key_kp_4 | key_kp_5 | key_kp_6 |
    key_kp_7 | key_kp_8 | key_kp_9 |
    key_kp_decimal |
    key_kp_divide |
    key_kp_multiply |
    key_kp_subtract |
    key_kp_add |
    key_kp_enter |
    key_kp_equal |
    key_left_shift |
    key_left_control |
    key_left_alt |
    key_left_super |
    key_right_shift |
    key_right_control |
    key_right_alt |
    key_right_super |
    key_menu
.
-doc "To be written.".
-type scancode() :: integer().

-doc "Mouse button IDs.".
-type mouse_button() ::
    mouse_button_1 |
    mouse_button_2 |
    mouse_button_3 |
    mouse_button_4 |
    mouse_button_5 |
    mouse_button_6 |
    mouse_button_7 |
    mouse_button_8 |
    mouse_button_left |
    mouse_button_right |
    mouse_button_middle
.

-doc "Joystick IDs.".
-type joystick() ::
    joystick_1 |
    joystick_2 |
    joystick_3 |
    joystick_4 |
    joystick_5 |
    joystick_6 |
    joystick_7 |
    joystick_8 |
    joystick_9 |
    joystick_10 |
    joystick_11 |
    joystick_12 |
    joystick_13 |
    joystick_14 |
    joystick_15 |
    joystick_16
.
-doc "Joystick hat states.".
-type joystick_hat() ::
    hat_centered |
    hat_up |
    hat_right |
    hat_down |
    hat_left |
    hat_right_up |
    hat_right_down |
    hat_left_up |
    hat_left_down
.

-doc "Gamepad axes.".
-type gamepad_axe() ::
    axe_left_x |
    axe_left_y |
    axe_right_x |
    axe_right_y |
    axe_left_trigger |
    axe_right_trigger
.
-doc "Gamepad buttons.".
-type gamepad_button() ::
    button_a |
    button_b |
    button_x |
    button_y |
    button_left_bumper |
    button_right_bumper |
    button_back |
    button_start |
    button_guide |
    button_left_thumb |
    button_right_thumb |
    button_dpad_up |
    button_dpad_right |
    button_dpad_down |
    button_dpad_left
.

-include("glfw.hrl").

init_nif() ->
    % The GLFW NIF module depends on the EGL NIF module, so we compute its
    % location first, then pass it to the GLFW NIF loader.
    EGLPrivDir = code:priv_dir(egl),
    EGLNifLocation = filename:join(EGLPrivDir, "beam-egl") ++ ".so",

    PrivDir = code:priv_dir(?MODULE),
    NifPath = filename:join(PrivDir, "beam-glfw"),
    ok = erlang:load_nif(NifPath, EGLNifLocation).

-doc """
Set an init hint.

It sets hints for the next initialization of GLFW.

The values you set hints to are never reset by GLFW, but they only take effect
during initialization. Once GLFW has been initialized, any values you set will
be ignored until the library is terminated and initialized again.

Some hints are platform specific. These may be set on any platform but they
will only affect their specific platform. Other platforms will ignore them.
Setting these hints requires no platform specific headers or functions.

> #### Possible Errors {: .error}
>
> - GLFW_INVALID_ENUM
> - GLFW_INVALID_VALUE

> #### Remarks {: .neutral}
>
> This function may be called before glfwInit.
""".
-doc(#{
    parameters => #{
        "Hint" => "The init hint to set.",
        "Value" => "The new value of the init hint."
    },
    return => "GLFW_TRUE if successful, or GLFW_FALSE if an error occurred.",
    see_also => [
        {glfw, init, 0},
        {glfw, terminate, 0}
    ],
    since => "3.3"
}).
-spec init_hint(init_hint_type(), init_hint_value()) -> ok.
init_hint(Hint, Value) ->
    {HintRaw, ValueRaw} = case Hint of
        platform ->
            ValueRaw_ = case
                Value of
                    any ->
                        ?GLFW_ANY_PLATFORM;
                    win32 ->
                        ?GLFW_PLATFORM_WIN32;
                    cocoa ->
                        ?GLFW_PLATFORM_COCOA;
                    wayland ->
                        ?GLFW_PLATFORM_WAYLAND;
                    x11 ->
                        ?GLFW_PLATFORM_X11;
                    null ->
                        ?GLFW_PLATFORM_NULL
            end,
            {?GLFW_PLATFORM, ValueRaw_};
        joystick_hat_buttons ->
            {?GLFW_JOYSTICK_HAT_BUTTONS, to_raw_bool(Value)};
        angle_platform_type ->
            ValueRaw_ = case Value of
                angle_platform_type_none ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_NONE;
                angle_platform_type_opengl ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_OPENGL;
                angle_platform_type_opengles ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_OPENGLES;
                angle_platform_type_d3d9 ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_D3D9;
                angle_platform_type_d3d11 ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_D3D11;
                angle_platform_type_vulkan ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_VULKAN;
                angle_platform_type_metal ->
                    ?GLFW_ANGLE_PLATFORM_TYPE_METAL
            end,
            {?GLFW_ANGLE_PLATFORM_TYPE, ValueRaw_};
        cocoa_chdir_resources ->
            {?GLFW_COCOA_CHDIR_RESOURCES, to_raw_bool(Value)};
        cocoa_menubar ->
            {?GLFW_COCOA_MENUBAR, to_raw_bool(Value)};
        wayland_libdecor ->
            ValueRaw_ = case Value of
                wayland_prefer_libdecor ->
                    ?GLFW_WAYLAND_PREFER_LIBDECOR;
                wayland_disable_libdecor ->
                    ?GLFW_WAYLAND_DISABLE_LIBDECOR
            end,
            {?GLFW_WAYLAND_LIBDECOR, ValueRaw_}
    end,
    init_hint_raw(HintRaw, ValueRaw).

init_hint_raw(_Hint, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Initialize the GLFW library.

It initializes the GLFW library. Before most GLFW functions can be used, GLFW
must be initialized, and before an application terminates GLFW should be
terminated in order to free any resources allocated during or after
initialization.

If this function fails, it calls `glfw:terminate/0` before returning. If it
succeeds, you should call `glfw:terminate/0` before the application exits.

Additional calls to this function after successful initialization but before
termination will return `true` immediately.

The `platform` init hint controls which platforms are considered during
initialization. This also depends on which platforms the library was compiled
to support.

> #### Possible Errors {: .error}
>
> - GLFW_PLATFORM_UNAVAILABLE
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> macOS: This function will change the current directory of the application to
> the Contents/Resources subdirectory of the application's bundle, if present.
> This can be disabled with the GLFW_COCOA_CHDIR_RESOURCES init hint.
>
> macOS: This function will create the main menu and dock icon for the
> application. If GLFW finds a MainMenu.nib it is loaded and assumed to contain
> a menu bar. Otherwise a minimal menu bar is created manually with common
> commands like Hide, Quit and About. The About entry opens a minimal about
> dialog with information from the application's bundle. The menu bar and dock
> icon can be disabled entirely with the GLFW_COCOA_MENUBAR init hint.
>
> Wayland, X11: If the library was compiled with support for both Wayland and
> X11, and the GLFW_PLATFORM init hint is set to GLFW_ANY_PLATFORM, the
> XDG_SESSION_TYPE environment variable affects which platform is picked. If
> the environment variable is not set, or is set to something other than
> wayland or x11, the regular detection mechanism will be used instead.
>
> X11: This function will set the LC_CTYPE category of the application locale
> according to the current environment if that category is still "C". This is
> because the "C" locale breaks Unicode text input.
""".
-doc(#{
    return => "GLFW_TRUE if successful, or GLFW_FALSE if an error occurred.",
    see_also => [
        {glfw, init_hint, 2},
        {glfw, terminate, 0}
    ]
}).
-spec init() -> boolean().
init() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Terminate the GLFW library.

It destroys all remaining windows and cursors, restores any modified gamma
ramps and frees any other allocated resources. Once this function is called,
you must again call `glfw:init/0` successfully before you will be able to use
most GLFW functions.

If GLFW has been successfully initialized, this function should be called
before the application exits. If initialization fails, there is no need to call
this function, as it is called by `glfw:init/0` before it returns failure.

This function has no effect if GLFW is not initialized.

See also: glfwInit

> #### Possible Errors {: .error}
>
> - GLFW_PLATFORM_UNAVAILABLE
> - GLFW_PLATFORM_ERROR

> #### Warning {: .warning}
>
> The contexts of any remaining windows must not be current on any other thread
> when this function is called.

> #### Remarks {: .neutral}
>
> This function may be called before glfwInit.

> #### Reentrancy {: .neutral}
>
> This function must not be called from a callback.
""".
-spec terminate() -> ok.
terminate() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the version of the GLFW library.

It retrieves the major, minor and revision numbers of the GLFW library. It is
intended for when you are using GLFW as a shared library and want to ensure
that you are using the minimum required version.

> #### Remarks {: .neutral}
>
> This function may be called before glfwInit.
""".
-doc(#{
    return => "The GLFW version numbers.",
    see_also => {glfw, version_string, 0},
    since => "1.0"
}).
-spec version() ->
    {Major :: integer(), Minor :: integer(), Revision ::integer}.
version() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
The ASCII encoded GLFW version string.

It returns the compile-time generated version string of the GLFW library
binary. It describes the version, platforms, compiler and any platform or
operating system specific compile-time options. It should not be confused with
the OpenGL or OpenGL ES version string, queried with any `gl:get_string/x`.

**Do not** use the version string to parse the GLFW library version. The
`glfw:version/0` function provides the version of the running library binary in
numerical format.

**Do not** use the version string to parse what platforms are supported. The
`glfw:platform_supported/0` function lets you query platform support.

> #### Remarks {: .neutral}
>
> This function may be called before glfwInit.
""".
-doc(#{
    return => "The ASCII encoded GLFW version string.",
    see_also => {glfw, version, 0},
    since => "3.0"
}).
-spec version_string() -> string().
version_string() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec get_error() ->
    {error, Code :: error_code(), Description :: error_description()} |
    no_error
.
get_error() ->
    {CodeRaw, Description} = get_error_raw(),
    case CodeRaw of
        ?GLFW_NO_ERROR ->
            no_error;
        _ ->
            Code = case CodeRaw of
                ?GLFW_NOT_INITIALIZED ->
                    not_initialized;
                ?GLFW_NO_CURRENT_CONTEXT ->
                    no_current_context;
                ?GLFW_INVALID_ENUM ->
                    invalid_enum;
                ?GLFW_INVALID_VALUE ->
                    invalid_value;
                ?GLFW_OUT_OF_MEMORY ->
                    out_of_memory;
                ?GLFW_API_UNAVAILABLE ->
                    api_unavailable;
                ?GLFW_VERSION_UNAVAILABLE ->
                    version_unavailable;
                ?GLFW_PLATFORM_ERROR ->
                    platform_error;
                ?GLFW_FORMAT_UNAVAILABLE ->
                    format_unavailable;
                ?GLFW_NO_WINDOW_CONTEXT ->
                    no_window_context;
                ?GLFW_CURSOR_UNAVAILABLE ->
                    cursor_unavailable;
                ?GLFW_FEATURE_UNAVAILABLE ->
                    feature_unavailable;
                ?GLFW_FEATURE_UNIMPLEMENTED ->
                    feature_unimplemented;
                ?GLFW_PLATFORM_UNAVAILABLE ->
                    platform_unavailable
            end,
            {error, Code, Description}
    end.

get_error_raw() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec error_handler() -> undefined | pid().
error_handler() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_error_handler(undefined | pid()) -> ok.
set_error_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the selected platform.

It returns the platform that was selected during initialization. The returned
value will be one of GLFW_PLATFORM_WIN32, GLFW_PLATFORM_COCOA,
GLFW_PLATFORM_WAYLAND, GLFW_PLATFORM_X11 or GLFW_PLATFORM_NULL.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
""".
-doc(#{
    return => "The currently selected platform, or zero if an error occurred.",
    see_also => {glfw, platform_supported, 1},
    since => "3.4"
}).
-spec platform() -> {ok, platform()} | error.
platform() ->
    Value = platform_raw(),
    case Value of
        0 ->
            error;
        ?GLFW_PLATFORM_WIN32 ->
            {ok, win32};
        ?GLFW_PLATFORM_COCOA ->
            {ok, cocoa};
        ?GLFW_PLATFORM_WAYLAND ->
            {ok, wayland};
        ?GLFW_PLATFORM_X11 ->
            {ok, x11};
        ?GLFW_PLATFORM_NULL ->
            {ok, null}
    end.

platform_raw() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Check if the platform is supported.

It returns whether the library was compiled with support for the specified
platform. The platform must be one of GLFW_PLATFORM_WIN32, GLFW_PLATFORM_COCOA,
GLFW_PLATFORM_WAYLAND, GLFW_PLATFORM_X11 or GLFW_PLATFORM_NULL.

> #### Possible Errors {: .error}
>
> - GLFW_INVALID_ENUM

> #### Remarks {: .neutral}
>
> This function may be called before glfwInit.
""".
-doc(#{
    parameters => #{
        "Platform" => "The platform to query."
    },
    return => "GLFW_TRUE if the platform is supported, or GLFW_FALSE otherwise.",
    see_also => {glfw, platform, 0},
    since => "3.4"
}).
-spec platform_supported(platform()) -> boolean().
platform_supported(Platform) ->
    Value = case Platform of
        win32 ->
            ?GLFW_PLATFORM_WIN32;
        cocoa ->
            ?GLFW_PLATFORM_COCOA;
        wayland ->
            ?GLFW_PLATFORM_WAYLAND;
        x11 ->
            ?GLFW_PLATFORM_X11;
        null ->
            ?GLFW_PLATFORM_NULL
    end,
    platform_supported_raw(Value).

platform_supported_raw(_Platform) ->
    erlang:nif_error(nif_library_not_loaded).

-doc"""
List of connected monitors.

It returns a list of handles for all connected monitors. The primary monitor is
always first in the returned list. If no monitors are found, the list if empty.

See also `primary_monitor/0`.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED

XXX
Pointer lifetime
    The returned array is allocated and freed by GLFW. You should not free it yourself. It is guaranteed to be valid only until the monitor configuration changes or the library is terminated.
""".
-doc(#{
    return => "An array of monitor handles, or NULL if no monitors were found or if an error occurred.",
    see_also => {glfw, primary_monitor, 0},
    since => "3.0"
}).
-spec monitors() -> [monitor()].
monitors() ->
    erlang:nif_error(nif_library_not_loaded).

-doc"""
Handle of the primary monitor.

It returns the primary monitor. This is usually the monitor where elements like
the task bar or global menu bar are located.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED

> #### Remarks {: .neutral}
>
> The primary monitor is always first in the array returned by glfwGetMonitors.
""".
-doc(#{
    return => "The primary monitor, or NULL if no monitors were found or if an error occurred.",
    see_also => {glfw, monitors, 0},
    since => "3.0"
}).
-spec primary_monitor() -> {ok, monitor()} | no_monitor.
primary_monitor() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the monitor position.

It returns the position, in screen coordinates, of the upper-left corner of the
specified monitor.

Any or all of the position arguments may be NULL. If an error occurs, all
non-NULL position arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The monitor coordinates, or NULL if an error occurred.",
    since => "3.0"
}).
-spec monitor_position(monitor()) -> {X :: integer(), Y :: integer()}.
monitor_position(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the monitor work area.

It returns the position, in screen coordinates, of the upper-left corner of the
work area of the specified monitor along with the work area size in screen
coordinates. The work area is defined as the area of the monitor not occluded
by the window system task bar where present. If no task bar exists then the
work area is the monitor resolution in screen coordinates.

Any or all of the position and size arguments may be NULL. If an error occurs,
all non-NULL position and size arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    % XXX
    return => "The monitor coordinates and size, or NULL if an error occurred.",
    since => "3.3"
}).
-spec monitor_work_area(monitor()) ->
    {X :: integer(), Y :: integer(), Width :: integer(), Height :: integer()}.
monitor_work_area(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the monitor physical size.

It returns the size, in millimetres, of the display area of the specified
monitor.

Some platforms do not provide accurate monitor size information, either because
the monitor EDID data is incorrect or because the driver does not report it
accurately.

Any or all of the size arguments may be NULL. If an error occurs, all non-NULL
size arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED

> #### Remarks {: .neutral}
>
> Windows: On Windows 8 and earlier the physical size is calculated from the
> current resolution and system DPI instead of querying the monitor EDID data.
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The monitor size, in millimeters, of the monitor's display area, or NULL if an error occurred.",
    since => "3.0"
}).
-spec monitor_physical_size(monitor()) ->
    {Width :: integer(), Height :: integer()}.
monitor_physical_size(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the monitor content scale.

It retrieves the content scale for the specified monitor. The content scale is
the ratio between the current DPI and the platform's default DPI. This is
especially important for text and any UI elements. If the pixel dimensions of
    your UI scaled by this look appropriate on your machine then it should
appear at a reasonable size on other machines regardless of their DPI and
scaling settings. This relies on the system DPI and scaling settings being
somewhat correct.

The content scale may depend on both the monitor resolution and pixel density
and on user settings. It may be very different from the raw DPI calculated from
the physical size and current resolution.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Wayland: Fractional scaling information is not yet available for monitors, so
> this function only returns integer content scales.
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The x and y axis content scale, or NULL if an error occurred.",
    see_also => {glfw, window_content_scale, 1},
    since => "3.3"
}).
-spec monitor_content_scale(monitor()) -> {X :: float(), Y :: float()}.
monitor_content_scale(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the monitor name.

It returns a human-readable name, encoded as UTF-8, of the specified monitor.
The name typically reflects the make and model of the monitor and is not
guaranteed to be unique among the connected monitors.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The UTF-8 encoded name of the monitor, or NULL if an error occurred.",
    since => "3.0"
}).
-spec monitor_name(monitor()) -> string().
monitor_name(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec monitor_handler(monitor()) -> undefined | pid().
monitor_handler(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec monitor_set_handler(monitor(), pid()) -> ok | not_ok.
monitor_set_handler(_Monitor, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return all video modes.

It returns an array of all video modes supported by the specified monitor. The
returned array is sorted in ascending order, first by color bit depth (the sum
of all channel depths), then by resolution area (the product of width and
height), then resolution width and finally by refresh rate.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "An array of video modes, or NULL if an error occurred.",
    see_also => {glfw, monitor_video_mode, 1},
    since => "1.0"
}).
-spec monitor_video_modes(monitor()) -> undefined | [#glfw_video_mode{}].
monitor_video_modes(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the current video mode.

It returns the current video mode of the specified monitor. If you have created
a full screen window for that monitor, the return value will depend on whether
that window is iconified.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The current mode of the monitor, or NULL if an error occurred.",
    see_also => {glfw, monitor_video_modes, 1},
    since => "3.0"
}).
-spec monitor_video_mode(monitor()) -> undefined | #glfw_video_mode{}.
monitor_video_mode(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the gamma ramp.

It generates an appropriately sized gamma ramp from the specified exponent and
then calls glfwSetGammaRamp with it. The value must be a finite number greater
than zero.

The software controlled gamma ramp is applied in addition to the hardware gamma
correction, which today is usually an approximation of sRGB gamma. This means
that setting a perfectly linear ramp, or gamma 1.0, will produce the default
(usually sRGB-like) behavior.

For gamma correct rendering with OpenGL or OpenGL ES, see the GLFW_SRGB_CAPABLE
hint.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE
> - GLFW_PLATFORM_ERROR
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Wayland: Gamma handling is a privileged protocol, this function will thus
> never be implemented and emits GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor whose gamma ramp to set.",
        "Gamma" => "The desired exponent."
    },
    % XXX
    return => "GLFW_TRUE if successful, or GLFW_FALSE if an error occurred.",
    since => "3.0"
}).
-spec monitor_set_gamma(monitor(), float()) -> ok | not_ok.
monitor_set_gamma(_Monitor, _Gamma) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the gamma ramp.

It returns the current gamma ramp of the specified monitor.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> Wayland: Gamma handling is a privileged protocol, this function will thus
> never be implemented and emits GLFW_FEATURE_UNAVAILABLE while returning NULL.
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor to query."
    },
    return => "The current gamma ramp, or NULL if an error occurred.",
    since => "3.0"
}).
-spec monitor_gamma_ramp(monitor()) -> undefined | #glfw_gamma_ramp{}.
monitor_gamma_ramp(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the gamma ramp.

It sets the current gamma ramp for the specified monitor. The original gamma
ramp for that monitor is saved by GLFW the first time this function is called
and is restored by glfwTerminate.

The software controlled gamma ramp is applied in addition to the hardware gamma
correction, which today is usually an approximation of sRGB gamma. This means
that setting a perfectly linear ramp, or gamma 1.0, will produce the default
(usually sRGB-like) behavior.

For gamma correct rendering with OpenGL or OpenGL ES, see the GLFW_SRGB_CAPABLE
hint.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> The size of the specified gamma ramp should match the size of the current
> ramp for that monitor.
> Windows: The gamma ramp size must be 256.
> Wayland: Gamma handling is a privileged protocol, this function will thus
> never be implemented and emits GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    parameters => #{
        "Monitor" => "The monitor whose gamma ramp to set.",
        "Ramp" => "The gamma ramp to use."
    },
    since => "3.0"
}).
-spec monitor_set_gamma_ramp(monitor(), #glfw_gamma_ramp{}) -> ok | not_ok.
monitor_set_gamma_ramp(_Monitor, _Ramp) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Create a window.

It creates a window and its associated OpenGL or OpenGL ES context. Most of the
options controlling how the window and its context should be created are
specified with window hints.

Successful creation does not change which context is current. Before you can
use the newly created context, you need to make it current. For information
about the share parameter, see Context object sharing.

The created window, framebuffer and context may differ from what you requested,
as not all parameters and hints are hard constraints. This includes the size of
the window, especially for full screen windows. To query the actual attributes
of the created window, framebuffer and context, see glfwGetWindowAttrib,
glfwGetWindowSize and glfwGetFramebufferSize.

To create a full screen window, you need to specify the monitor the window will
cover. If no monitor is specified, the window will be windowed mode. Unless you
have a way for the user to choose a specific monitor, it is recommended that
you pick the primary monitor. For more information on how to query connected
monitors, see Retrieving monitors.

For full screen windows, the specified size becomes the resolution of the
window's desired video mode. As long as a full screen window is not iconified,
the supported video mode most closely matching the desired video mode is set
for the specified monitor. For more information about full screen windows,
including the creation of so called windowed full screen or borderless full
screen windows, see "Windowed full screen" windows.

Once you have created the window, you can switch it between windowed and full
screen mode with glfwSetWindowMonitor. This will not affect its OpenGL or
OpenGL ES context.

By default, newly created windows use the placement recommended by the window
system. To create the window at a specific position, set the GLFW_POSITION_X
and GLFW_POSITION_Y window hints before creation. To restore the default
behavior, set either or both hints back to GLFW_ANY_POSITION.

As long as at least one full screen window is not iconified, the screensaver is
prohibited from starting.

Window systems put limits on window sizes. Very large or very small window
dimensions may be overridden by the window system on creation. Check the actual
size after creation.

The swap interval is not set during window creation and the initial value may
vary depending on driver settings and defaults.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
> - GLFW_INVALID_VALUE
> - GLFW_API_UNAVAILABLE
> - GLFW_VERSION_UNAVAILABLE
> - GLFW_FORMAT_UNAVAILABLE
> - GLFW_NO_WINDOW_CONTEXT
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Windows: Window creation will fail if the Microsoft GDI software OpenGL
> implementation is the only one available.
>
> Windows: If the executable has an icon resource named GLFW_ICON, it will be
> set as the initial icon for the window. If no such icon is present, the
> IDI_APPLICATION icon will be used instead. To set a different icon, see
> glfwSetWindowIcon.
>
> Windows: The context to share resources with must not be current on any
> other thread.
>
> macOS: The OS only supports core profile contexts for OpenGL versions 3.2
> and later. Before creating an OpenGL context of version 3.2 or later you
> must set the GLFW_OPENGL_PROFILE hint accordingly. OpenGL 3.0 and 3.1
> contexts are not supported at all on macOS.
>
> macOS: The GLFW window has no icon, as it is not a document window, but the
> dock icon will be the same as the application bundle's icon. For more
> information on bundles, see the Bundle Programming Guide in the Mac Developer
> Library.
>
> macOS: On OS X 10.10 and later the window frame will not be rendered at full
> resolution on Retina displays unless the GLFW_SCALE_FRAMEBUFFER hint is
> GLFW_TRUE and the NSHighResolutionCapable key is enabled in the application
> bundle's Info.plist. For more information, see High Resolution Guidelines for
> OS X in the Mac Developer Library. The GLFW test and example programs use a
> custom Info.plist template for this, which can be found as
> CMake/Info.plist.in in the source tree.
>
> macOS: When activating frame autosaving with GLFW_COCOA_FRAME_NAME, the
> specified window size and position may be overridden by previously saved
> values.
>
> Wayland: GLFW uses libdecor where available to create its window decorations.
> This in turn uses server-side XDG decorations where available and provides
> high quality client-side decorations on compositors like GNOME. If both XDG
> decorations and libdecor are unavailable, GLFW falls back to a very simple
> set of window decorations that only support moving, resizing and the window
> manager's right-click menu.
>
> X11: Some window managers will not respect the placement of initially hidden
> windows.
>
> X11: Due to the asynchronous nature of X11, it may take a moment for a
> window to reach its requested state. This means you may not be able to query
> the final size, position or other attributes directly after window creation.
>
> X11: The class part of the WM_CLASS window property will by default be set
> to the window title passed to this function. The instance part will use the
> contents of the RESOURCE_NAME environment variable, if present and not
> empty, or fall back to the window title. Set the GLFW_X11_CLASS_NAME and
> GLFW_X11_INSTANCE_NAME window hints to override this.
""".
-doc(#{
    parameters => #{
        "With" => "The desired width, in screen coordinates, of the window. This must be greater than zero.",
        "Height" => "The desired height, in screen coordinates, of the window. This must be greater than zero.",
        "Title" => "The initial, UTF-8 encoded window title.",
        "Monitor" => "The monitor to use for full screen mode, or NULL for windowed mode.",
        "Share" => "The window whose context to share resources with, or NULL to not share resources."
    },
    return => "The handle of the created window, or NULL if an error occurred.",
    see_also => {glfw, destroy_window, 1}
}).
-spec create_window(integer(), integer(), string()) ->
    {ok, window()} | no_window.
create_window(_Width, _Height, _Title) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Destroy a window.

It destroys the specified window and its context. On calling this function, no
further callbacks will be called for that window.

If the context of the specified window is current on the main thread, it is
detached before being destroyed.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR

> #### Note {: .info}
>
> TThe context of the specified window must not be current on any other thread
> when this function is called.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to destroy."
    },
    see_also => {glfw, create_window, 3}
}).
-spec destroy_window(window()) -> ok.
destroy_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the value of the close flag.

It returns the value of the close flag of the specified window.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
""".
-doc(#{
    parameters => #{
        "Window" => "The window to query."
    },
    return => "The value of the close flag."
}).
-spec window_should_close(window()) -> boolean().
window_should_close(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the value of the close flag.

It sets the value of the close flag of the specified window. This can be used
to override the user's attempt to close the window, or to signal that it
should be closed.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
""".
-doc(#{
    parameters => #{
        "Window" => "The window whose flag to change.",
        "Value" => "The new value."
    }
}).
-spec set_window_should_close(window(), boolean()) -> ok.
set_window_should_close(_Window, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the window title.

It returns the window title, encoded as UTF-8, of the specified window. This is
the title set previously by glfwCreateWindow or glfwSetWindowTitle.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED

> #### Remarks {: .neutral}
>
> The returned title is currently a copy of the title last set by
> glfwCreateWindow or glfwSetWindowTitle. It does not include any additional
> text which may be appended by the platform or another program.

Pointer lifetime
    The returned string is allocated and freed by GLFW. You should not free it yourself. It is valid until the next call to glfwGetWindowTitle or glfwSetWindowTitle, or until the library is terminated.

""".
-doc(#{
    parameters => #{
        "Window" => "The window to query."
    },
    return => "The UTF-8 encoded window title, or NULL if an error occurred.",
    see_also => {glfw, set_window_title, 2}
}).
-spec window_title(window()) -> string().
window_title(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window title.

It sets the window title, encoded as UTF-8, of the specified window.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
x
> macOS: The window title will not be updated until the next time you process
> events.
""".
-doc(#{
    parameters => #{
        "Window" => "The window whose title to change.",
        "Title" => "The UTF-8 encoded window title."
    },
    see_also => {glfw, window_title, 1}
}).
-spec set_window_title(window(), string()) -> ok.
set_window_title(_Window, _Title) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window icon.

It sets the icon of the specified window. If passed an array of candidate
images, those of or closest to the sizes desired by the system are selected. If
no images are specified, the window reverts to its default icon.

The pixels are 32-bit, little-endian, non-premultiplied RGBA, i.e. eight bits
per channel with the red channel first. They are arranged canonically as packed
sequential rows, starting from the top-left corner.

The desired image sizes varies depending on platform and system settings. The
selected images will be rescaled as needed. Good sizes include 16x16, 32x32 and
48x48.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> macOS: Regular windows do not have icons on macOS. This function will emit
> GLFW_FEATURE_UNAVAILABLE. The dock icon will be the same as the application
> bundle's icon. For more information on bundles, see the Bundle Programming
> Guide in the Mac Developer Library.
>
> Wayland: There is no existing protocol to change an icon, the window will
> thus inherit the one defined in the application's desktop file. This function
> will emit GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    parameters => #{
        % XXX
        "Window" => "The window whose icon to set.",
        "Count" => "The number of images in the specified array, or zero to revert to the default window icon.",
        "Images" => "The images to create the icon from. This is ignored if count is zero."
    },
    since => "3.2"
}).
-spec set_window_icon(window(), term()) -> ok.
set_window_icon(_Window, _Icon) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the window position.

It retrieves the position, in screen coordinates, of the upper-left corner of
the content area of the specified window.

Any or all of the position arguments may be NULL. If an error occurs, all
non-NULL position arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> Wayland: There is no way for an application to retrieve the global position
> of its windows. This function will emit GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    % XXX
    parameters => #{
        "Window" => "The window to query."
    },
    return => "The x- and y-coordinates of the upper-left corner of the content area, in screen coordinates.",
    see_also => {glfw, set_window_position, 2},
    since => "3.0"
}).
-spec window_position(window()) -> {X :: integer(), Y :: integer()}.
window_position(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window position.

It sets the position, in screen coordinates, of the upper-left corner of the
content area of the specified windowed mode window. If the window is a full
screen window, this function does nothing.

Do not use this function to move an already visible window unless you have very
good reasons for doing so, as it will confuse and annoy the user.

The window manager may put limits on what positions are allowed. GLFW cannot
and should not override these limits.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> Wayland: There is no way for an application to set the global position of its
> windows. This function will emit GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to move.",
        "Position" => "The coordinate of the upper-left corner of the content area."
    },
    see_also => {glfw, window_position, 0}
}).
-spec set_window_position(window(), {X :: integer(), Y :: integer()}) -> ok.
set_window_position(_Window, _Position) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the window size.

It retrieves the size, in screen coordinates, of the content area of the
specified window. If you wish to retrieve the size of the framebuffer of the
window in pixels, see glfwGetFramebufferSize.

Any or all of the size arguments may be NULL. If an error occurs, all non-NULL
size arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window whose size to retrieve."
    },
    return => "The width and height of the content area, in screen coordinates.",
    see_also => {glfw, set_window_size, 2}
}).
-spec window_size(window()) -> {Width :: integer(), Height :: integer()}.
window_size(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window size.

It sets the size, in screen coordinates, of the content area of the specified
window.

For full screen windows, this function updates the resolution of its desired
video mode and switches to the video mode closest to it, without affecting the
window's context. As the context is unaffected, the bit depths of the
framebuffer remain unchanged.

If you wish to update the refresh rate of the desired video mode in addition to
its resolution, see glfwSetWindowMonitor.

The window manager may put limits on what sizes are allowed. GLFW cannot and
should not override these limits.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to resize.",
        "Size" => "The desired width and height, in screen coordinates, of the window content area."
    },
    see_also => [
        {glfw, window_size, 0},
        {glfw, set_window_monitor, 4}
    ]
}).
-spec set_window_size(window(), {Width :: integer(), Height :: integer()}) -> ok.
set_window_size(_Window, _Size) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window size limits.

It sets the size limits of the content area of the specified window. If the
window is full screen, the size limits only take effect once it is made
windowed. If the window is not resizable, this function does nothing.

The size limits are applied immediately to a windowed mode window and may cause
it to be resized.

The maximum dimensions must be greater than or equal to the minimum dimensions
and all must be greater than or equal to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> If you set size limits and an aspect ratio that conflict, the results are
> undefined.
>
> Wayland: The size limits will not be applied until the window is actually
> resized, either by the user or by the compositor.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to set limits for.",
        "MinSize" => "The minimum size, in screen coordinates, of the content area, or GLFW_DONT_CARE.",
        "MaxSize" => "The maximum size, in screen coordinates, of the content area, or GLFW_DONT_CARE."
    },
    see_also => {glfw, set_window_aspect_ratio, 2},
    since => "3.2"
}).
-spec set_window_size_limits(window(), size_limits(), size_limits()) -> ok.
set_window_size_limits(Window, MinSize, MaxSize) ->
    {MinWidth, MinHeight} = unpack_dont_care_vector2(MinSize),
    {MaxWidth, MaxHeight} = unpack_dont_care_vector2(MaxSize),
    set_window_size_limits_raw(Window, MinWidth, MinHeight, MaxWidth, MaxHeight).

set_window_size_limits_raw(_Window, _MinWidth, _MinHeight, _MaxWidth, _MaxHeight) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window aspect ratio.

It sets the required aspect ratio of the content area of the specified window.
If the window is full screen, the aspect ratio only takes effect once it is
made windowed. If the window is not resizable, this function does nothing.

The aspect ratio is specified as a numerator and a denominator and both values
must be greater than zero. For example, the common 16:9 aspect ratio is
specified as 16 and 9, respectively.

If the numerator and denominator is set to GLFW_DONT_CARE then the aspect ratio
limit is disabled.

The aspect ratio is applied immediately to a windowed mode window and may cause
it to be resized.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> If you set size limits and an aspect ratio that conflict, the results are
> undefined.
>
> Wayland: The aspect ratio will not be applied until the window is actually
> resized, either by the user or by the compositor.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to set limits for.",
        % XXX
        "Ratio" => "The desired aspect ratio, or GLFW_DONT_CARE."
    },
    see_also => {glfw, set_window_size_limits, 3},
    since => "3.2"
}).
-spec set_window_aspect_ratio(window(), aspect_ratio()) -> ok.
set_window_aspect_ratio(Window, Ratio) ->
    {Numerator, Denominator} = unpack_dont_care_vector2(Ratio),
    set_window_aspect_ratio_raw(Window, Numerator, Denominator).

set_window_aspect_ratio_raw(_Window, _Numerator, _Denominator) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the size of the window frame.

It retrieves the size, in screen coordinates, of each edge of the frame of the
specified window. This size includes the title bar, if the window has one. The
size of the frame may vary depending on the window-related hints used to create
it.

Because this function retrieves the size of each window frame edge and not the
offset along a particular coordinate axis, the retrieved values will always be
zero or positive.

Any or all of the size arguments may be NULL. If an error occurs, all non-NULL
size arguments will be set to zero.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window whose frame size to query."
    },
    % XXX
    return => "The size of the window frame, in screen coordinates.",
    since => "3.1"
}).
-spec window_frame_size(window()) ->
    {Left :: integer(), Top :: integer(), Right :: integer(), Bottom :: integer()}.
window_frame_size(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the content scale.

It retrieves the content scale for the specified window. The content scale is
the ratio between the current DPI and the platform's default DPI. This is
especially important for text and any UI elements. If the pixel dimensions of
    your UI scaled by this look appropriate on your machine then it should
appear at a reasonable size on other machines regardless of their DPI and
scaling settings. This relies on the system DPI and scaling settings being
somewhat correct.

On platforms where each monitors can have its own content scale, the window
content scale will depend on which monitor the system considers the window to
be on.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to query."
    },
    return => "The x- and y-axis content scale of the specified window.",
    see_also => [
        % XXX
        {glfw, set_window_content_scale_callback, 1},
        {glfw, monitor_content_scale, 1}
    ],
    since => "3.3"
}).
-spec window_content_scale(window()) -> {X :: float(), Y :: float()}.
window_content_scale(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the window opacity.

It returns the opacity of the window, including any decorations.

The opacity (or alpha) value is a positive finite number between zero and one,
where zero is fully transparent and one is fully opaque. If the system does not
support whole window transparency, this function always returns one.

The initial opacity value for newly created windows is one.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to query."
    },
    return => "The opacity value of the specified window.",
    see_also => {glfw, set_window_opacity, 2},
    since => "3.3"
}).
-spec window_opacity(window()) -> float().
window_opacity(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the window opacity.

It sets the opacity of the window, including any decorations.

The opacity (or alpha) value is a positive finite number between zero and one,
where zero is fully transparent and one is fully opaque.

The initial opacity value for newly created windows is one.

A window created with framebuffer transparency may not use whole window
transparency. The results of doing this are undefined.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> Wayland: There is no way to set an opacity factor for a window. This function
> will emit GLFW_FEATURE_UNAVAILABLE.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to set the opacity for.",
        "Opacity" => "The desired opacity of the specified window."
    },
    see_also => {glfw, window_opacity, 0},
    since => "3.3"
}).
-spec set_window_opacity(window(), float()) -> ok.
set_window_opacity(_Window, _Opacity) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Iconify the window.

It iconifies (minimizes) the specified window if it was previously restored. If
the window is already iconified, this function does nothing.

If the specified window is a full screen window, GLFW restores the original
video mode of the monitor. The window's desired video mode is set again when
the window is restored.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Wayland: Once a window is iconified, glfwRestoreWindow won't be able to
> restore it. This is a design decision of the xdg-shell protocol.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to iconify."
    },
    see_also => [
        {glfw, restore_window, 1},
        {glfw, maximize_window, 1}
    ],
    since => "2.1"
}).
-spec iconify_window(window()) -> ok.
iconify_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Restore the window.

It restores the specified window if it was previously iconified (minimized) or
maximized. If the window is already restored, this function does nothing.

If the specified window is an iconified full screen window, its desired video
mode is set again for its monitor when the window is restored.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to restore."
    },
    see_also => [
        {glfw, iconify_window, 1},
        {glfw, maximize_window, 1}
    ],
    since => "2.1"
}).
-spec restore_window(window()) -> ok.
restore_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Maximize the window.

It maximizes the specified window if it was previously not maximized. If the
window is already maximized, this function does nothing.

If the specified window is a full screen window, this function does nothing.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to maximize."
    },
    see_also => [
        {glfw, iconify_window, 1},
        {glfw, restore_window, 1}
    ],
    since => "3.2"
}).
-spec maximize_window(window()) -> ok.
maximize_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Show the window.

It makes the specified window visible if it was previously hidden. If the
window is already visible or is in full screen mode, this function does
nothing.

By default, windowed mode windows are focused when shown Set the
GLFW_FOCUS_ON_SHOW window hint to change this behavior for all newly created
windows, or change the behavior for an existing window with
glfwSetWindowAttrib.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Wayland: Because Wayland wants every frame of the desktop to be complete,
> this function does not immediately make the window visible. Instead it will
> become visible the next time the window framebuffer is updated after this
> call.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to make visible."
    },
    see_also => {glfw, hide_window, 1},
    since => "3.0"
}).
-spec show_window(window()) -> ok.
show_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Hide the window.

It hides the specified window if it was previously visible. If the window is
already hidden or is in full screen mode, this function does nothing.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to hide."
    },
    see_also => {glfw, show_window, 1},
    since => "3.0"
}).
-spec hide_window(window()) -> ok.
hide_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set input focus to the window.

It brings the specified window to front and sets input focus. The window should
already be visible and not iconified.

By default, both windowed and full screen mode windows are focused when
initially created. Set the GLFW_FOCUSED to disable this behavior.

Also by default, windowed mode windows are focused when shown with
glfwShowWindow. Set the GLFW_FOCUS_ON_SHOW to disable this behavior.

Do not use this function to steal focus from other applications unless you are
certain that is what the user wants. Focus stealing can be extremely
disruptive.

For a less disruptive way of getting the user's attention, see attention
requests.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> Wayland: The compositor will likely ignore focus requests unless another
> window created by the same application already has input focus.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to give input focus."
    },
    since => "3.2"
}).
-spec focus_window(window()) -> ok.
focus_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Request user attention to the window.

It requests user attention to the specified window. On platforms where this is
not supported, attention is requested to the application as a whole.

Once the user has given attention, usually by focusing the window or
application, the system will end the request automatically.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR'

> #### Remarks {: .neutral}
>
> macOS: Attention is requested to the application as a whole, not the specific
> window.
""".
-doc(#{
    parameters => #{
        "Window" => "The window to request attention to."
    },
    since => "3.3"
}).
-spec request_window_attention(window()) -> ok.
request_window_attention(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_position_handler(window()) -> undefined | pid().
window_position_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_position_handler(window(), undefined | pid()) -> ok.
set_window_position_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_size_handler(window()) -> undefined | pid().
window_size_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_size_handler(window(), undefined | pid()) -> ok.
set_window_size_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_close_handler(window()) -> undefined | pid().
window_close_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_close_handler(window(), undefined | pid()) -> ok.
set_window_close_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_refresh_handler(window()) -> undefined | pid().
window_refresh_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_refresh_handler(window(), undefined | pid()) -> ok.
set_window_refresh_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_focus_handler(window()) -> undefined | pid().
window_focus_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_focus_handler(window(), undefined | pid()) -> ok.
set_window_focus_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_iconify_handler(window()) -> undefined | pid().
window_iconify_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_iconify_handler(window(), undefined | pid()) -> ok.
set_window_iconify_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_maximize_handler(window()) -> undefined | pid().
window_maximize_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_maximize_handler(window(), undefined | pid()) -> ok.
set_window_maximize_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec window_content_scale_handler(window()) -> undefined | pid().
window_content_scale_handler(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_window_content_scale_handler(window(), undefined | pid()) -> ok.
set_window_content_scale_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Poll events.

It processes only those events that are already in the event queue and then
returns immediately. Processing events will cause the window and input
callbacks associated with those events to be called.

On some platforms, a window move, resize or menu operation will cause event
processing to block. This is due to how event processing is designed on those
platforms. You can use the window refresh callback to redraw the contents of
your window when necessary during such operations.

Do not assume that callbacks you set will only be called in response to event
processing functions like this one. While it is necessary to poll for events,
window systems that require GLFW to register callbacks of its own can pass
events to GLFW in response to many window system function calls. GLFW will pass
those events on to the application callbacks before returning.

Event processing is not required for joystick input to work.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    see_also => [
        % XXX: Do they actually exist ?
        {glfw, wait_events, 0},
        {glfw, wait_events_timeout, 1}
    ]
}).
-spec poll_events() -> ok.
poll_events() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Post an empty event.

It posts an empty event from the current thread to the event queue, causing
glfwWaitEvents or glfwWaitEventsTimeout to return.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    see_also => [
        % XXX: Do they actually exist ?
        {glfw, wait_events, 0},
        {glfw, wait_events_timeout, 1}
    ]
}).
-spec post_empty_event() -> ok.
post_empty_event() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the value of an input option.

It returns the value of an input option for the specified window. The mode must
be one of GLFW_CURSOR, GLFW_STICKY_KEYS, GLFW_STICKY_MOUSE_BUTTONS,
GLFW_LOCK_KEY_MODS or GLFW_RAW_MOUSE_MOTION.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
""".
-doc(#{
    parameters => #{
        "Window" => "The window to query.",
        "Mode" => "One of GLFW_CURSOR, GLFW_STICKY_KEYS, GLFW_STICKY_MOUSE_BUTTONS, GLFW_LOCK_KEY_MODS or GLFW_RAW_MOUSE_MOTION."
    },
    return => "The value of the specified input mode.",
    see_also => {glfw, set_input_mode, 3},
    since => "3.0"
}).
-spec input_mode(window(), input_mode()) -> ok.
input_mode(Window, Mode) ->
    ModeRaw = case Mode of
        cursor ->
            ?GLFW_CURSOR;
        sticky_keys ->
            ?GLFW_STICKY_KEYS;
        sticky_mouse_buttons ->
            ?GLFW_STICKY_MOUSE_BUTTONS;
        lock_key_mods ->
            ?GLFW_LOCK_KEY_MODS;
        raw_mouse_motion ->
            ?GLFW_RAW_MOUSE_MOTION
    end,
    ValueRaw = input_mode_raw(Window, ModeRaw),
    case Mode of
        cursor ->
            case ValueRaw of
                ?GLFW_CURSOR_NORMAL ->
                    normal;
                ?GLFW_CURSOR_HIDDEN ->
                    hidden;
                ?GLFW_CURSOR_DISABLED ->
                    disabled;
                ?GLFW_CURSOR_CAPTURED ->
                    captured
            end;
        _ ->
            case ValueRaw of
                ?GLFW_FALSE ->
                    false;
                ?GLFW_TRUE ->
                    true
            end
    end.

input_mode_raw(_Window, _Mode) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set an input mode option.

It sets an input mode option for the specified window. The mode must be one of
GLFW_CURSOR, GLFW_STICKY_KEYS, GLFW_STICKY_MOUSE_BUTTONS, GLFW_LOCK_KEY_MODS or
GLFW_RAW_MOUSE_MOTION.

If the mode is GLFW_CURSOR, the value must be one of the following cursor
modes:

- GLFW_CURSOR_NORMAL makes the cursor visible and behaving normally.
- GLFW_CURSOR_HIDDEN makes the cursor invisible when it is over the content area of the window but does not restrict the cursor from leaving.
- GLFW_CURSOR_DISABLED hides and grabs the cursor, providing virtual and unlimited cursor movement. This is useful for implementing for example 3D camera controls.
- GLFW_CURSOR_CAPTURED makes the cursor visible and confines it to the content area of the window.

If the mode is GLFW_STICKY_KEYS, the value must be either GLFW_TRUE to enable
sticky keys, or GLFW_FALSE to disable it. If sticky keys are enabled, a key
press will ensure that glfwGetKey returns GLFW_PRESS the next time it is called
even if the key had been released before the call. This is useful when you are
only interested in whether keys have been pressed but not when or in which
order.

If the mode is GLFW_STICKY_MOUSE_BUTTONS, the value must be either GLFW_TRUE to
enable sticky mouse buttons, or GLFW_FALSE to disable it. If sticky mouse
buttons are enabled, a mouse button press will ensure that glfwGetMouseButton
returns GLFW_PRESS the next time it is called even if the mouse button had been
released before the call. This is useful when you are only interested in
whether mouse buttons have been pressed but not when or in which order.

If the mode is GLFW_LOCK_KEY_MODS, the value must be either GLFW_TRUE to enable
lock key modifier bits, or GLFW_FALSE to disable them. If enabled, callbacks
that receive modifier bits will also have the GLFW_MOD_CAPS_LOCK bit set when
the event was generated with Caps Lock on, and the GLFW_MOD_NUM_LOCK bit when
Num Lock was on.

If the mode is GLFW_RAW_MOUSE_MOTION, the value must be either GLFW_TRUE to
enable raw (unscaled and unaccelerated) mouse motion when the cursor is
disabled, or GLFW_FALSE to disable it. If raw motion is not supported,
attempting to set this will emit GLFW_FEATURE_UNAVAILABLE. Call
glfwRawMouseMotionSupported to check for support.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE
""".
-doc(#{
    parameters => #{
        "Window" => "The window whose input mode to set.",
        "Mode" => "One of GLFW_CURSOR, GLFW_STICKY_KEYS, GLFW_STICKY_MOUSE_BUTTONS, GLFW_LOCK_KEY_MODS or GLFW_RAW_MOUSE_MOTION.",
        "Value" => "The new value of the specified input mode."
    },
    see_also => {glfw, input_mode, 2},
    since => "3.0"
}).
-spec set_input_mode(window(), input_mode(), input_mode_value()) -> ok.
set_input_mode(Window, cursor, Value) ->
    ValueRaw = case Value of
        normal ->
            ?GLFW_CURSOR_NORMAL;
        hidden ->
            ?GLFW_CURSOR_HIDDEN;
        disabled ->
            ?GLFW_CURSOR_DISABLED;
        captured ->
            ?GLFW_CURSOR_CAPTURED
    end,
    set_input_mode_raw(Window, ?GLFW_CURSOR, ValueRaw);
set_input_mode(Window, sticky_keys, Value) ->
    set_input_mode_raw(Window, ?GLFW_STICKY_KEYS, to_raw_bool(Value));
set_input_mode(Window, sticky_mouse_buttons, Value) ->
    set_input_mode_raw(Window, ?GLFW_STICKY_MOUSE_BUTTONS, to_raw_bool(Value));
set_input_mode(Window, lock_key_mods, Value) ->
    set_input_mode_raw(Window, ?GLFW_LOCK_KEY_MODS, to_raw_bool(Value));
set_input_mode(Window, raw_mouse_motion, Value) ->
    set_input_mode_raw(Window, ?GLFW_RAW_MOUSE_MOTION, to_raw_bool(Value)).

set_input_mode_raw(_Window, _Mode, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Create a custom cursor.

It creates a new custom cursor image that can be set for a window with
glfwSetCursor. The cursor can be destroyed with glfwDestroyCursor. Any
remaining cursors are destroyed by glfwTerminate.

The pixels are 32-bit, little-endian, non-premultiplied RGBA, i.e. eight bits
per channel with the red channel first. They are arranged canonically as packed
sequential rows, starting from the top-left corner.

The cursor hotspot is specified in pixels, relative to the upper-left corner of
the cursor image. Like all other coordinate systems in GLFW, the X-axis points
to the right and the Y-axis points down.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Image" => "The desired cursor image.",
        "Hotspot" => "The desired x- and y-coordinates, in pixels, of the cursor hotspot."
    },
    return => "The handle of the created cursor, or NULL if an error occurred.",
    see_also => [
        {glfw, destroy_cursor, 1},
        {glfw, create_standard_cursor, 1}
    ],
    since => "3.1"
}).
-spec create_cursor(#glfw_image{}, {integer(), integer()}) ->
    {ok, cursor()} | error.
create_cursor(Image, Hotspot) ->
    #glfw_image{
        width = Width,
        height = Height,
        pixels = Pixels
    } = Image,
    {X, Y} = Hotspot,
    create_cursor_raw(Width, Height, Pixels, X, Y).

create_cursor_raw(_Width, _Height, _Pixels, _HotspotX, _HotspotY) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Create a standard cursor.

It returns a cursor with a standard shape, that can be set for a window with
glfwSetCursor. The images for these cursors come from the system cursor theme
and their exact appearance will vary between platforms.

Most of these shapes are guaranteed to exist on every supported platform but a
few may not be present. See the table below for details.

- Cursor shape 	Windows 	macOS 	X11 	Wayland
- GLFW_ARROW_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_IBEAM_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_CROSSHAIR_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_POINTING_HAND_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_RESIZE_EW_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_RESIZE_NS_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_RESIZE_NWSE_CURSOR 	Yes 	Yes1 	Maybe2 	Maybe2
- GLFW_RESIZE_NESW_CURSOR 	Yes 	Yes1 	Maybe2 	Maybe2
- GLFW_RESIZE_ALL_CURSOR 	Yes 	Yes 	Yes 	Yes
- GLFW_NOT_ALLOWED_CURSOR 	Yes 	Yes 	Maybe2 	Maybe2

1) This uses a private system API and may fail in the future.

2) This uses a newer standard that not all cursor themes support.

If the requested shape is not available, this function emits a
GLFW_CURSOR_UNAVAILABLE error and returns NULL.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
> - GLFW_FEATURE_UNAVAILABLE
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Shape" => "One of the standard shapes."
    },
    return => "A new cursor ready to use, or NULL if an error occurred.",
    see_also => {glfw, create_cursor, 2},
    since => "3.1"
}).
-spec create_standard_cursor(cursor_shape()) -> {ok, cursor()} | error.
create_standard_cursor(Shape) ->
    ShapeRaw = case
        Shape of
            arrow ->
                ?GLFW_ARROW_CURSOR;
            ibeam ->
                ?GLFW_IBEAM_CURSOR;
            crosshair ->
                ?GLFW_CROSSHAIR_CURSOR;
            pointing_hand ->
                ?GLFW_POINTING_HAND_CURSOR;
            resize_ew ->
                ?GLFW_RESIZE_EW_CURSOR;
            resize_ns ->
                ?GLFW_RESIZE_NS_CURSOR;
            resize_nwse ->
                ?GLFW_RESIZE_NWSE_CURSOR;
            resize_nesw ->
                ?GLFW_RESIZE_NESW_CURSOR;
            resize_all ->
                ?GLFW_RESIZE_ALL_CURSOR;
            not_allowed ->
                ?GLFW_NOT_ALLOWED_CURSOR
    end,
    create_standard_cursor_raw(ShapeRaw).

create_standard_cursor_raw(_Shape) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Destroy a custom cursor.

It destroys a cursor previously created with glfwCreateCursor. Any remaining
cursors will be destroyed by glfwTerminate.

If the specified cursor is current for any window, that window will be reverted
to the default cursor. This does not affect the cursor mode.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Cursor" => "The cursor to destroy."
    },
    see_also => {glfw, create_cursor, 2},
    since => "3.1"
}).
-spec destroy_cursor(cursor()) -> ok.
destroy_cursor(_Cursor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the cursor for a window.

It sets the cursor image to be used when the cursor is over the content area of
the specified window. The set cursor will only be visible when the cursor mode
of the window is GLFW_CURSOR_NORMAL.

On some platforms, the set cursor may not be visible unless the window also has
input focus.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-doc(#{
    parameters => #{
        "Window" => "The window to set the cursor for.",
        "Cursor" => "The cursor to set, or NULL to switch back to the default arrow cursor."
    },
    since => "3.1"
}).
-spec set_cursor(window(), default | cursor()) -> ok.
set_cursor(_Window, _Cursor) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return whether raw mouse motion is supported.

It returns whether raw mouse motion is supported on the current system. This
status does not change after GLFW has been initialized so you only need to
check this once. If you attempt to enable raw motion on a system that does not
support it, GLFW_PLATFORM_ERROR will be emitted.

Raw mouse motion is closer to the actual motion of the mouse across a surface.
It is not affected by the scaling and acceleration applied to the motion of the
desktop cursor. That processing is suitable for a cursor while raw motion is
better for controlling for example a 3D camera. Because of this, raw mouse
motion is only provided when the cursor is disabled.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
""".
-doc(#{
    return => "GLFW_TRUE if raw mouse motion is supported on the current machine, or GLFW_FALSE otherwise.",
    see_also => {glfw, set_input_mode, 3},
    since => "3.3"
}).
-spec raw_mouse_motion_supported() -> boolean().
raw_mouse_motion_supported() ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Returns the name of a key.

It returns the name of the specified printable key, encoded as UTF-8. This is
typically the character that key would produce without any modifier keys,
intended for displaying key bindings to the user. For dead keys, it is
typically the diacritic it would add to a character.

Do not use this function for text input. You will break text input for many
languages even if it happens to work for yours.

If the key is GLFW_KEY_UNKNOWN, the scancode is used to identify the key,
otherwise the scancode is ignored. If you specify a non-printable key, or
GLFW_KEY_UNKNOWN and a scancode that maps to a non-printable key, this
function returns NULL but does not emit an error.

This behavior allows you to always pass in the arguments in the key callback
without modification.

The printable keys are:

- GLFW_KEY_APOSTROPHE
- GLFW_KEY_COMMA
- GLFW_KEY_MINUS
- GLFW_KEY_PERIOD
- GLFW_KEY_SLASH
- GLFW_KEY_SEMICOLON
- GLFW_KEY_EQUAL
- GLFW_KEY_LEFT_BRACKET
- GLFW_KEY_RIGHT_BRACKET
- GLFW_KEY_BACKSLASH
- GLFW_KEY_WORLD_1
- GLFW_KEY_WORLD_2
- GLFW_KEY_0 to GLFW_KEY_9
- GLFW_KEY_A to GLFW_KEY_Z
- GLFW_KEY_KP_0 to GLFW_KEY_KP_9
- GLFW_KEY_KP_DECIMAL
- GLFW_KEY_KP_DIVIDE
- GLFW_KEY_KP_MULTIPLY
- GLFW_KEY_KP_SUBTRACT
- GLFW_KEY_KP_ADD
- GLFW_KEY_KP_EQUAL

Names for printable keys depend on keyboard layout, while names for
non-printable keys are the same across layouts but depend on the application
language and should be localized along with other user interface text.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_VALUE
> - GLFW_INVALID_ENUM
> - GLFW_PLATFORM_ERROR

> #### Remarks {: .neutral}
>
> The contents of the returned string may change when a keyboard layout change
> event is received.
""".
-doc(#{
    parameters => #{
        "Key" => "The key to query, or GLFW_KEY_UNKNOWN.",
        "Scancode" => "The scancode of the key to query."
    },
    return => "The UTF-8 encoded, layout-specific name of the key, or NULL.",
    see_also => [
        {glfw, key_scancode, 1},
        {glfw, key, 1}
    ],
    since => "3.2"
}).
-spec key_name({key, key()} | {scancode, scancode()}) -> undefined | string().
key_name({key, Key}) ->
    KeyRaw = to_raw_key(Key),
    key_name_key(KeyRaw);
key_name({scancode, Scancode}) ->
    key_name_scancode(Scancode).

key_name_key(_Key) ->
    erlang:nif_error(nif_library_not_loaded).

key_name_scancode(_Scancode) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the scancode of a key.

It returns the platform-specific scancode of the specified key.

If the specified key token corresponds to a physical key not supported on the
current platform then this method will return -1. Calling this function with
anything other than a key token will return -1 and generate a GLFW_INVALID_ENUM
error.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
""".
-doc(#{
    parameters => #{
        "Key" => "Any key token."
    },
    return => "The platform-specific scancode for the key, or -1 if the key is not supported on the current platform or an error occurred.",
    see_also => [
        {glfw, key_name, 2},
        {glfw, key, 1}
    ],
    since => "3.3"
}).
-spec key_scancode(key()) -> undefined | scancode().
key_scancode(Key) ->
    KeyRaw = to_raw_key(Key),
    ScanscodeRaw = key_scancode_raw(KeyRaw),
    case ScanscodeRaw of
        -1 ->
            undefined;
        _ ->
            ScanscodeRaw
    end.

key_scancode_raw(_Key) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the last state of a key.

It returns the last state reported for the specified key to the specified
window. The returned state is one of GLFW_PRESS or GLFW_RELEASE. The action
GLFW_REPEAT is only reported to the key callback.

If the GLFW_STICKY_KEYS input mode is enabled, this function returns GLFW_PRESS
the first time you call it for a key that was pressed, even if that key has
already been released.

The key functions deal with physical keys, with key tokens named after their
use on the standard US keyboard layout. If you want to input text, use the
Unicode character callback instead.

The modifier key bit masks are not key tokens and cannot be used with this
function.

Do not use this function to implement text input.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
""".
-doc(#{
    parameters => #{
        "Window" => "The desired window.",
        "Key" => "The desired keyboard key. GLFW_KEY_UNKNOWN is not a valid key for this function."
    },
    return => "One of GLFW_PRESS or GLFW_RELEASE.",
    see_also => [
        {glfw, get_key_name, 1},
        {glfw, get_key_scancode, 1}
    ],
    since => "1.0"
}).
-spec key(window(), key()) -> press | release.
key(Window, Key) ->
    KeyRaw = to_raw_key(Key),
    key_raw(Window, KeyRaw).

key_raw(_Window, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the last state of a mouse button.

It returns the last state reported for the specified mouse button to the
specified window. The returned state is one of `press` or `release`.

If the GLFW_STICKY_MOUSE_BUTTONS input mode is enabled, this function returns
GLFW_PRESS the first time you call it for a mouse button that was pressed, even
if that mouse button has already been released.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
""".
-doc(#{
    parameters => #{
        "Window" => "The desired window.",
        "Button" => "The desired mouse button."
    },
    return => "One of GLFW_PRESS or GLFW_RELEASE."
}).
-spec mouse_button(window(), mouse_button()) -> press | release.
mouse_button(Window, Button) ->
    ButtonRaw = to_raw_mouse_button(Button),
    mouse_button_raw(Window, ButtonRaw).

mouse_button_raw(_Window, _Button) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Return the cursor position.

It returns the position of the cursor, in screen coordinates, relative to the
upper-left corner of the content area of the specified window.

If the cursor is disabled then the cursor position is unbounded and limited
only by the minimum and maximum values of a double.

The coordinate can be converted to their integer equivalents with the floor
function. Casting directly to an integer type works for positive coordinates,
but fails for negative ones.

See also: `glfw:set_cursor_position/x`.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
""".
-spec cursor_position(window()) -> {X :: float(), Y :: float()}.
cursor_position(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Set the cursor position.

It sets the position, in screen coordinates, of the cursor relative to the
upper-left corner of the content area of the specified window. The window must
have input focus. If the window does not have input focus when this function is
called, it fails silently.

Do not use this function to implement things like camera controls. GLFW already
provides the "disabled cursor" mode that hides the cursor, transparently
re-centers it and provides unconstrained cursor motion. See
glfw:set_input_mode/0` for more information.

If the cursor mode is disabled then the cursor position is unconstrained and
limited only by the minimum and maximum values of a double.

See also: glfwGetCursorPos

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_PLATFORM_ERROR
> - GLFW_FEATURE_UNAVAILABLE

> #### Remarks {: .neutral}
>
> Wayland: This function will only work when the cursor mode is "disabled",
> otherwise it will emit GLFW_FEATURE_UNAVAILABLE.
""".
-spec set_cursor_position(window(), {X :: float(), Y :: float()}) -> ok.
set_cursor_position(Window, Position) ->
    {X, Y} = Position,
    set_cursor_position_raw(Window, X, Y).

set_cursor_position_raw(_Window, _X, _Y) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec key_handler(window()) -> undefined | pid().
key_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_key_handler(window(), undefined | pid()) -> ok.
set_key_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec char_handler(window()) -> undefined | pid().
char_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_char_handler(window(), undefined | pid()) -> ok.
set_char_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec char_mods_handler(window()) -> undefined | pid().
char_mods_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_char_mods_handler(window(), undefined | pid()) -> ok.
set_char_mods_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec mouse_button_handler(window()) -> undefined | pid().
mouse_button_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_mouse_button_handler(window(), undefined | pid()) -> ok.
set_mouse_button_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec cursor_position_handler(window()) -> undefined | pid().
cursor_position_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_cursor_position_handler(window(), undefined | pid()) -> ok.
set_cursor_position_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec cursor_enter_handler(window()) -> undefined | pid().
cursor_enter_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_cursor_enter_handler(window(), undefined | pid()) -> ok.
set_cursor_enter_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec scroll_handler(window()) -> undefined | pid().
scroll_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_scroll_handler(window(), undefined | pid()) -> ok.
set_scroll_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec drop_handler(window()) -> undefined | pid().
drop_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec set_drop_handler(window(), undefined | pid()) -> ok.
set_drop_handler(_Window, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
Returns whether the specified joystick is present.

It returns whether the specified joystick is present. There is no need to call
this function before other functions that accept a joystick ID, as they all
check for presence before performing any other work.

> #### Possible Errors {: .error}
>
> - GLFW_NOT_INITIALIZED
> - GLFW_INVALID_ENUM
> - GLFW_PLATFORM_ERROR
""".
-spec joystick_present(joystick()) -> boolean().
joystick_present(Joystick) ->
    JoystickRaw = to_raw_joystick(Joystick),
    joystick_present_raw(JoystickRaw).

joystick_present_raw(_Joystick) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec get_joystick_axes(joystick()) -> not_present | [float()].
get_joystick_axes(Joystick) ->
    JoystickRaw = to_raw_joystick(Joystick),
    get_joystick_axes_raw(JoystickRaw).

get_joystick_axes_raw(_Joystick) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec get_joystick_buttons(joystick()) -> not_present | [release | press].
get_joystick_buttons(Joystick) ->
    JoystickRaw = to_raw_joystick(Joystick),
    get_joystick_buttons_raw(JoystickRaw).

get_joystick_buttons_raw(_Joystick) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec get_joystick_hats(joystick()) -> not_present | [integer()].
get_joystick_hats(Joystick) ->
    JoystickRaw = to_raw_joystick(Joystick),
    get_joystick_hats_raw(JoystickRaw).

get_joystick_hats_raw(_Joystick) ->
    erlang:nif_error(nif_library_not_loaded).

-doc """
To be written.

To be written.
""".
-spec get_joystick_guid(joystick()) -> not_present | binary().
get_joystick_guid(Joystick) ->
    JoystickRaw = to_raw_joystick(Joystick),
    get_joystick_guid_raw(JoystickRaw).

get_joystick_guid_raw(_Joystick) ->
    erlang:nif_error(nif_library_not_loaded).

window_egl_handle(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

unpack_dont_care_vector2(Vector2) ->
    case Vector2 of
        dont_care ->
            {?GLFW_DONT_CARE, ?GLFW_DONT_CARE};
        {dont_care, dont_care} ->
            {?GLFW_DONT_CARE, ?GLFW_DONT_CARE};
        {dont_care, Y} ->
            {?GLFW_DONT_CARE, Y};
        {X, dont_care} ->
            {X, ?GLFW_DONT_CARE};
        {X, Y} ->
            {X, Y}
    end.

to_raw_bool(Value) ->
    case Value of
        true ->
            ?GLFW_TRUE;
        false ->
            ?GLFW_FALSE
    end.

to_raw_key(Key) ->
    case Key of
        key_space -> ?GLFW_KEY_SPACE;
        key_apostrophe -> ?GLFW_KEY_APOSTROPHE;
        key_comma -> ?GLFW_KEY_COMMA;
        key_minus -> ?GLFW_KEY_MINUS;
        key_period -> ?GLFW_KEY_PERIOD;
        key_slash -> ?GLFW_KEY_SLASH;
        key_0 -> ?GLFW_KEY_0;
        key_1 -> ?GLFW_KEY_1;
        key_2 -> ?GLFW_KEY_2;
        key_3 -> ?GLFW_KEY_3;
        key_4 -> ?GLFW_KEY_4;
        key_5 -> ?GLFW_KEY_5;
        key_6 -> ?GLFW_KEY_6;
        key_7 -> ?GLFW_KEY_7;
        key_8 -> ?GLFW_KEY_8;
        key_9 -> ?GLFW_KEY_9;
        key_semicolon -> ?GLFW_KEY_SEMICOLON;
        key_equal -> ?GLFW_KEY_EQUAL;
        key_a -> ?GLFW_KEY_A;
        key_b -> ?GLFW_KEY_B;
        key_c -> ?GLFW_KEY_C;
        key_d -> ?GLFW_KEY_D;
        key_e -> ?GLFW_KEY_E;
        key_f -> ?GLFW_KEY_F;
        key_g -> ?GLFW_KEY_G;
        key_h -> ?GLFW_KEY_H;
        key_i -> ?GLFW_KEY_I;
        key_j -> ?GLFW_KEY_J;
        key_k -> ?GLFW_KEY_K;
        key_l -> ?GLFW_KEY_L;
        key_m -> ?GLFW_KEY_M;
        key_n -> ?GLFW_KEY_N;
        key_o -> ?GLFW_KEY_O;
        key_p -> ?GLFW_KEY_P;
        key_q -> ?GLFW_KEY_Q;
        key_r -> ?GLFW_KEY_R;
        key_s -> ?GLFW_KEY_S;
        key_t -> ?GLFW_KEY_T;
        key_u -> ?GLFW_KEY_U;
        key_v -> ?GLFW_KEY_V;
        key_w -> ?GLFW_KEY_W;
        key_x -> ?GLFW_KEY_X;
        key_y -> ?GLFW_KEY_Y;
        key_z -> ?GLFW_KEY_Z;
        key_left_bracket -> ?GLFW_KEY_LEFT_BRACKET;
        key_backslash -> ?GLFW_KEY_BACKSLASH;
        key_right_bracket -> ?GLFW_KEY_RIGHT_BRACKET;
        key_grave_accent -> ?GLFW_KEY_GRAVE_ACCENT;
        key_world_1 -> ?GLFW_KEY_WORLD_1;
        key_world_2 -> ?GLFW_KEY_WORLD_2;
        key_escape -> ?GLFW_KEY_ESCAPE;
        key_enter -> ?GLFW_KEY_ENTER;
        key_tab -> ?GLFW_KEY_TAB;
        key_backspace -> ?GLFW_KEY_BACKSPACE;
        key_insert -> ?GLFW_KEY_INSERT;
        key_delete -> ?GLFW_KEY_DELETE;
        key_right -> ?GLFW_KEY_RIGHT;
        key_left -> ?GLFW_KEY_LEFT;
        key_down -> ?GLFW_KEY_DOWN;
        key_up -> ?GLFW_KEY_UP;
        key_page_up -> ?GLFW_KEY_PAGE_UP;
        key_page_down -> ?GLFW_KEY_PAGE_DOWN;
        key_home -> ?GLFW_KEY_HOME;
        key_end -> ?GLFW_KEY_END;
        key_caps_lock -> ?GLFW_KEY_CAPS_LOCK;
        key_scroll_lock -> ?GLFW_KEY_SCROLL_LOCK;
        key_num_lock -> ?GLFW_KEY_NUM_LOCK;
        key_print_screen -> ?GLFW_KEY_PRINT_SCREEN;
        key_pause -> ?GLFW_KEY_PAUSE;
        key_f1 -> ?GLFW_KEY_F1;
        key_f2 -> ?GLFW_KEY_F2;
        key_f3 -> ?GLFW_KEY_F3;
        key_f4 -> ?GLFW_KEY_F4;
        key_f5 -> ?GLFW_KEY_F5;
        key_f6 -> ?GLFW_KEY_F6;
        key_f7 -> ?GLFW_KEY_F7;
        key_f8 -> ?GLFW_KEY_F8;
        key_f9 -> ?GLFW_KEY_F9;
        key_f10 -> ?GLFW_KEY_F10;
        key_f11 -> ?GLFW_KEY_F11;
        key_f12 -> ?GLFW_KEY_F12;
        key_f13 -> ?GLFW_KEY_F13;
        key_f14 -> ?GLFW_KEY_F14;
        key_f15 -> ?GLFW_KEY_F15;
        key_f16 -> ?GLFW_KEY_F16;
        key_f17 -> ?GLFW_KEY_F17;
        key_f18 -> ?GLFW_KEY_F18;
        key_f19 -> ?GLFW_KEY_F19;
        key_f20 -> ?GLFW_KEY_F20;
        key_f21 -> ?GLFW_KEY_F21;
        key_f22 -> ?GLFW_KEY_F22;
        key_f23 -> ?GLFW_KEY_F23;
        key_f24 -> ?GLFW_KEY_F24;
        key_f25 -> ?GLFW_KEY_F25;
        key_kp_0 -> ?GLFW_KEY_KP_0;
        key_kp_1 -> ?GLFW_KEY_KP_1;
        key_kp_2 -> ?GLFW_KEY_KP_2;
        key_kp_3 -> ?GLFW_KEY_KP_3;
        key_kp_4 -> ?GLFW_KEY_KP_4;
        key_kp_5 -> ?GLFW_KEY_KP_5;
        key_kp_6 -> ?GLFW_KEY_KP_6;
        key_kp_7 -> ?GLFW_KEY_KP_7;
        key_kp_8 -> ?GLFW_KEY_KP_8;
        key_kp_9 -> ?GLFW_KEY_KP_9;
        key_kp_decimal -> ?GLFW_KEY_KP_DECIMAL;
        key_kp_divide -> ?GLFW_KEY_KP_DIVIDE;
        key_kp_multiply -> ?GLFW_KEY_KP_MULTIPLY;
        key_kp_subtract -> ?GLFW_KEY_KP_SUBTRACT;
        key_kp_add -> ?GLFW_KEY_KP_ADD;
        key_kp_enter -> ?GLFW_KEY_KP_ENTER;
        key_kp_equal -> ?GLFW_KEY_KP_EQUAL;
        key_left_shift -> ?GLFW_KEY_LEFT_SHIFT;
        key_left_control -> ?GLFW_KEY_LEFT_CONTROL;
        key_left_alt -> ?GLFW_KEY_LEFT_ALT;
        key_left_super -> ?GLFW_KEY_LEFT_SUPER;
        key_right_shift -> ?GLFW_KEY_RIGHT_SHIFT;
        key_right_control -> ?GLFW_KEY_RIGHT_CONTROL;
        key_right_alt -> ?GLFW_KEY_RIGHT_ALT;
        key_right_super -> ?GLFW_KEY_RIGHT_SUPER;
        key_menu -> ?GLFW_KEY_MENU
    end.

to_raw_mouse_button(MouseButton) ->
    case MouseButton of
        mouse_button_1 -> ?GLFW_MOUSE_BUTTON_1;
        mouse_button_2 -> ?GLFW_MOUSE_BUTTON_2;
        mouse_button_3 -> ?GLFW_MOUSE_BUTTON_3;
        mouse_button_4 -> ?GLFW_MOUSE_BUTTON_4;
        mouse_button_5 -> ?GLFW_MOUSE_BUTTON_5;
        mouse_button_6 -> ?GLFW_MOUSE_BUTTON_6;
        mouse_button_7 -> ?GLFW_MOUSE_BUTTON_7;
        mouse_button_8 -> ?GLFW_MOUSE_BUTTON_8;
        mouse_button_left -> ?GLFW_MOUSE_BUTTON_LEFT;
        mouse_button_right -> ?GLFW_MOUSE_BUTTON_RIGHT;
        mouse_button_middle -> ?GLFW_MOUSE_BUTTON_MIDDLE
    end.

to_raw_joystick(Joystick) ->
    case Joystick of
        joystick_1 -> ?GLFW_JOYSTICK_1;
        joystick_2 -> ?GLFW_JOYSTICK_2;
        joystick_3 -> ?GLFW_JOYSTICK_3;
        joystick_4 -> ?GLFW_JOYSTICK_4;
        joystick_5 -> ?GLFW_JOYSTICK_5;
        joystick_6 -> ?GLFW_JOYSTICK_6;
        joystick_7 -> ?GLFW_JOYSTICK_7;
        joystick_8 -> ?GLFW_JOYSTICK_8;
        joystick_9 -> ?GLFW_JOYSTICK_9;
        joystick_10 -> ?GLFW_JOYSTICK_10;
        joystick_11 -> ?GLFW_JOYSTICK_11;
        joystick_12 -> ?GLFW_JOYSTICK_12;
        joystick_13 -> ?GLFW_JOYSTICK_13;
        joystick_14 -> ?GLFW_JOYSTICK_14;
        joystick_15 -> ?GLFW_JOYSTICK_15;
        joystick_16 -> ?GLFW_JOYSTICK_16
    end.
