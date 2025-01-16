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
