%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_test).
-include_lib("eunit/include/eunit.hrl").

glfw_test() ->
    {Major, Minor, Revision} = glfw:version(),
    ?assert(erlang:is_integer(Major)),
    ?assert(erlang:is_integer(Minor)),
    ?assert(erlang:is_integer(Revision)),

    Version = glfw:version_string(),
    ?assert(erlang:is_list(Version)),

    false = glfw:platform_supported(win32),
    false = glfw:platform_supported(cocoa),
    false = glfw:platform_supported(wayland),
    true = glfw:platform_supported(x11),
    false = glfw:platform_supported(null),

    error = glfw:platform(),

    ok = test_init_hint(),

    glfw:init(),
    {ok, x11} = glfw:platform(),

    ok.

test_init_hint() ->
    ok = glfw:init_hint(platform, any),
    ok = glfw:init_hint(platform, win32),
    ok = glfw:init_hint(platform, cocoa),
    ok = glfw:init_hint(platform, wayland),
    ok = glfw:init_hint(platform, x11),
    ok = glfw:init_hint(platform, null),

    ok = glfw:init_hint(joystick_hat_buttons, false),
    ok = glfw:init_hint(joystick_hat_buttons, true),

    ok = glfw:init_hint(angle_platform_type, angle_platform_type_none),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_opengl),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_opengles),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_d3d9),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_d3d11),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_vulkan),
    ok = glfw:init_hint(angle_platform_type, angle_platform_type_metal),

    ok = glfw:init_hint(cocoa_chdir_resources, false),
    ok = glfw:init_hint(cocoa_chdir_resources, true),

    ok = glfw:init_hint(cocoa_menubar, false),
    ok = glfw:init_hint(cocoa_menubar, true),

    ok = glfw:init_hint(wayland_libdecor, wayland_prefer_libdecor),
    ok = glfw:init_hint(wayland_libdecor, wayland_disable_libdecor),

    ok.
