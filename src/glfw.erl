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

-export_type([monitor_event/0]).

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

-nifs([init_hint/2]).
-nifs([init/0]).
-nifs([terminate/0]).
-nifs([version/0]).
-nifs([version_string/0]).
-nifs([last_error/0]).
-nifs([error_handler/0]).
-nifs([set_error_handler/1]).
-nifs([platform/0]).
-nifs([platform_supported/1]).

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

-on_load(init_nif/0).

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

-type monitor_event() :: connected | disconnected.

-include("glfw.hrl").

init_nif() ->
    ok = erlang:load_nif("./priv/glfw", 0).

-spec init_hint(init_hint_type(), init_hint_value()) -> ok.
init_hint(_Hint, _Value) ->
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

-spec platform() -> platform().
platform() ->
    erlang:nif_error(nif_library_not_loaded).

-spec platform_supported(platform()) -> boolean().
platform_supported(_Platform) ->
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
