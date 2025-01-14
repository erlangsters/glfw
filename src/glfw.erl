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

init_nif() ->
    ok = erlang:load_nif("./priv/glfw", 0).

-spec init_hint(init_hint_type(), init_hint_value()) -> ok.
init_hint(_Hint, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

-spec init() -> boolean().
init() ->
    erlang:nif_error(nif_library_not_loaded).

-spec terminate() -> ok().
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
