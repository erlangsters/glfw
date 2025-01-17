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

-export_type([platform/0]).

-export_type([init_hint_type/0]).
-export_type([init_hint_value/0]).

-export_type([error_type/0]).

-export_type([monitor/0]).
-export_type([window/0]).
-export_type([cursor/0]).

-export_type([monitor_event/0]).

-export_type([size_limits/0]).
-export_type([aspect_ratio/0]).

-export_type([cursor_shape/0]).

-export_type([key/0]).
-export_type([scancode/0]).

-export([init_hint/2]).
-export([init/0]).
-export([terminate/0]).
-export([version/0]).
-export([version_string/0]).
-export([last_error/0]).
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

-export([poll_events/0]).
-export([post_empty_event/0]).

-export([create_cursor/2]).
-export([create_standard_cursor/1]).
-export([destroy_cursor/1]).
-export([set_cursor/2]).

-export([raw_mouse_motion_supported/0]).
-export([key_name/1]).
-export([key_scancode/1]).

-nifs([init_hint_raw/2]).
-nifs([init/0]).
-nifs([terminate/0]).
-nifs([version/0]).
-nifs([version_string/0]).
-nifs([last_error/0]).
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

-nifs([poll_events/0]).
-nifs([post_empty_event/0]).

-nifs([create_cursor_raw/5]).
-nifs([create_standard_cursor_raw/1]).
-nifs([destroy_cursor/1]).
-nifs([set_cursor/2]).

-nifs([raw_mouse_motion_supported/0]).
-nifs([key_name_key/1]).
-nifs([key_name_scancode/1]).
-nifs([key_scancode_raw/1]).

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

-type platform() :: win32 | cocoa | wayland | x11 | null.

-type joystick_hat_buttons_hint_value() :: boolean().
-type platform_hint_value() :: any | platform().
-type angle_platform_hint_value() ::
    angle_platform_type_none |
    angle_platform_type_opengl |
    angle_platform_type_opengles |
    angle_platform_type_d3d9 |
    angle_platform_type_d3d11 |
    angle_platform_type_vulkan |
    angle_platform_type_metal
.
-type cocoa_chdir_resources_hint_value() :: boolean().
-type cocoa_menubar_hint_value() :: boolean().
-type way_libdecor_hint_value() ::
    wayland_prefer_libdecor |
    wayland_disable_libdecor
.

-type init_hint_type() ::
    platform |
    joystick_hat_buttons |
    angle_platform_type |
    cocoa_chdir_resources |
    cocoa_menubar |
    wayland_libdecor
.
-type init_hint_value() ::
    platform_hint_value() |
    joystick_hat_buttons_hint_value() |
    angle_platform_hint_value() |
    cocoa_chdir_resources_hint_value() |
    cocoa_menubar_hint_value() |
    way_libdecor_hint_value()
.

-type error_type() ::
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
-type scancode() :: integer().

-include("glfw.hrl").

init_nif() ->
    ok = erlang:load_nif("./priv/glfw", 0).

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
            ValueRaw_ = case Value of
                true ->
                    ?GLFW_TRUE;
                false ->
                    ?GLFW_FALSE
            end,
            {?GLFW_JOYSTICK_HAT_BUTTONS, ValueRaw_};
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
            ValueRaw_ = case Value of
                true ->
                    ?GLFW_TRUE;
                false ->
                    ?GLFW_FALSE
            end,
            {?GLFW_COCOA_CHDIR_RESOURCES, ValueRaw_};
        cocoa_menubar ->
            ValueRaw_ = case Value of
                true ->
                    ?GLFW_TRUE;
                false ->
                    ?GLFW_FALSE
            end,
            {?GLFW_COCOA_MENUBAR, ValueRaw_};
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

-spec init() -> boolean().
init() ->
    erlang:nif_error(nif_library_not_loaded).

-spec terminate() -> ok.
terminate() ->
    erlang:nif_error(nif_library_not_loaded).

-spec version() ->
    {Major :: integer(), Minor :: integer(), Revision ::integer}.
version() ->
    erlang:nif_error(nif_library_not_loaded).

-spec version_string() -> string().
version_string() ->
    erlang:nif_error(nif_library_not_loaded).

-spec last_error() -> {error, Description :: string()} | no_error.
last_error() ->
    erlang:nif_error(nif_library_not_loaded).

-spec error_handler() -> undefined | pid().
error_handler() ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_error_handler(pid()) -> ok | not_ok.
set_error_handler(_Handler) ->
    erlang:nif_error(nif_library_not_loaded).

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

-spec monitors() -> [monitor()].
monitors() ->
    erlang:nif_error(nif_library_not_loaded).

-spec primary_monitor() -> {ok, monitor()} | no_monitor.
primary_monitor() ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_position(monitor()) -> {X :: integer(), Y :: integer()}.
monitor_position(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_work_area(monitor()) ->
    {X :: integer(), Y :: integer(), Width :: integer(), Height :: integer()}.
monitor_work_area(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_physical_size(monitor()) ->
    {Width :: integer(), Height :: integer()}.
monitor_physical_size(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_content_scale(monitor()) -> {X :: float(), Y :: float()}.
monitor_content_scale(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_name(monitor()) -> string().
monitor_name(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_handler(monitor()) -> undefined | pid().
monitor_handler(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_set_handler(monitor(), pid()) -> ok | not_ok.
monitor_set_handler(_Monitor, _Handler) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_video_modes(monitor()) -> undefined | [#glfw_video_mode{}].
monitor_video_modes(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_video_mode(monitor()) -> undefined | #glfw_video_mode{}.
monitor_video_mode(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_set_gamma(monitor(), float()) -> ok | not_ok.
monitor_set_gamma(_Monitor, _Gamma) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_gamma_ramp(monitor()) -> undefined | #glfw_gamma_ramp{}.
monitor_gamma_ramp(_Monitor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec monitor_set_gamma_ramp(monitor(), #glfw_gamma_ramp{}) -> ok | not_ok.
monitor_set_gamma_ramp(_Monitor, _Ramp) ->
    erlang:nif_error(nif_library_not_loaded).

-spec create_window(integer(), integer(), string()) ->
    {ok, window()} | no_window.
create_window(_Width, _Height, _Title) ->
    erlang:nif_error(nif_library_not_loaded).

-spec destroy_window(window()) -> ok.
destroy_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_should_close(window()) -> boolean().
window_should_close(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_should_close(window(), boolean()) -> ok.
set_window_should_close(_Window, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_title(window()) -> string().
window_title(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_title(window(), string()) -> ok.
set_window_title(_Window, _Title) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_icon(window(), term()) -> ok.
set_window_icon(_Window, _Icon) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_position(window()) -> {X :: integer(), Y :: integer()}.
window_position(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_position(window(), {X :: integer(), Y :: integer()}) -> ok.
set_window_position(_Window, _Position) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_size(window()) -> {Width :: integer(), Height :: integer()}.
window_size(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_size(window(), {Width :: integer(), Height :: integer()}) -> ok.
set_window_size(_Window, _Size) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_size_limits(window(), size_limits(), size_limits()) -> ok.
set_window_size_limits(Window, MinSize, MaxSize) ->
    {MinWidth, MinHeight} = unpack_dont_care_vector2(MinSize),
    {MaxWidth, MaxHeight} = unpack_dont_care_vector2(MaxSize),
    set_window_size_limits_raw(Window, MinWidth, MinHeight, MaxWidth, MaxHeight).

set_window_size_limits_raw(_Window, _MinWidth, _MinHeight, _MaxWidth, _MaxHeight) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_aspect_ratio(window(), aspect_ratio()) -> ok.
set_window_aspect_ratio(Window, Ratio) ->
    {Numerator, Denominator} = unpack_dont_care_vector2(Ratio),
    set_window_aspect_ratio_raw(Window, Numerator, Denominator).

set_window_aspect_ratio_raw(_Window, _Numerator, _Denominator) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_frame_size(window()) ->
    {Left :: integer(), Top :: integer(), Right :: integer(), Bottom :: integer()}.
window_frame_size(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_content_scale(window()) -> {X :: float(), Y :: float()}.
window_content_scale(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec window_opacity(window()) -> float().
window_opacity(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_window_opacity(window(), float()) -> ok.
set_window_opacity(_Window, _Opacity) ->
    erlang:nif_error(nif_library_not_loaded).

-spec iconify_window(window()) -> ok.
iconify_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec restore_window(window()) -> ok.
restore_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec maximize_window(window()) -> ok.
maximize_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec show_window(window()) -> ok.
show_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec hide_window(window()) -> ok.
hide_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec focus_window(window()) -> ok.
focus_window(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec request_window_attention(window()) -> ok.
request_window_attention(_Window) ->
    erlang:nif_error(nif_library_not_loaded).

-spec poll_events() -> ok.
poll_events() ->
    erlang:nif_error(nif_library_not_loaded).

-spec post_empty_event() -> ok.
post_empty_event() ->
    erlang:nif_error(nif_library_not_loaded).

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

-spec destroy_cursor(cursor()) -> ok.
destroy_cursor(_Cursor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec set_cursor(window(), default | cursor()) -> ok.
set_cursor(_Window, _Cursor) ->
    erlang:nif_error(nif_library_not_loaded).

-spec raw_mouse_motion_supported() -> boolean().
raw_mouse_motion_supported() ->
    erlang:nif_error(nif_library_not_loaded).

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
