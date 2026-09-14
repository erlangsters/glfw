%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_window_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("glfw/include/glfw.hrl").

glfw_window_test() ->
    no_window = glfw:create_window(800, 600, "Hello, World!"),

    true = glfw:init(),

    {ok, Window} = glfw:create_window(800, 600, "Hello, World!"),

    false = glfw:window_should_close(Window),
    ok = glfw:set_window_should_close(Window, true),
    true = glfw:window_should_close(Window),
    ok = glfw:set_window_should_close(Window, false),

    "Hello, World!" = glfw:window_title(Window),
    ok = glfw:set_window_title(Window, "Goodbye, World!"),
    "Goodbye, World!" = glfw:window_title(Window),

    ok = glfw:set_window_icon(Window, [
        #glfw_image{width = 1, height = 1, pixels = <<255, 0, 0, 255>>}
    ]),
    ok = glfw:set_window_icon(Window, []),

    {X, Y} = glfw:window_position(Window),
    io:format(user, "window position (x: ~p, y: ~p)~n", [X, Y]),
    ok = glfw:set_window_position(Window, {100, 100}),
    % XXX: Check if the position was actually updated.

    {Width, Height} = glfw:window_size(Window),
    io:format(user, "window size (width: ~p, height: ~p)~n", [Width, Height]),
    {FbWidth, FbHeight} = glfw:framebuffer_size(Window),
    io:format(user, "framebuffer size (width: ~p, height: ~p)~n", [FbWidth, FbHeight]),
    ?assert(erlang:is_integer(FbWidth)),
    ?assert(erlang:is_integer(FbHeight)),
    Handle = glfw:window_egl_handle(Window),
    ?assert(Handle =/= error),
    ok = probe_egl_window_surface(Handle),
    ok = glfw:set_window_size(Window, {1024, 768}),
    % XXX: Check if the size was actually updated.

    ok = glfw:set_window_size_limits(Window, {640, 480}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {dont_care, 480}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, dont_care}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {dont_care, dont_care}, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, dont_care, {1920, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {dont_care, 1080}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {1920, dont_care}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, {dont_care, dont_care}),
    ok = glfw:set_window_size_limits(Window, {640, 480}, dont_care),

    ok = glfw:set_window_aspect_ratio(Window, {16, 9}),
    ok = glfw:set_window_aspect_ratio(Window, {dont_care, 9}),
    ok = glfw:set_window_aspect_ratio(Window, {16, dont_care}),
    ok = glfw:set_window_aspect_ratio(Window, {dont_care, dont_care}),

    {Left, Top, Right, Bottom} = glfw:window_frame_size(Window),
    io:format(user, "window frame size (left: ~p, top: ~p, right: ~p, bottom: ~p)~n", [Left, Top, Right, Bottom]),

    {XScale, YScale} = glfw:window_content_scale(Window),
    io:format(user, "window content scale (x: ~p, y: ~p)~n", [XScale, YScale]),

    Opacity = glfw:window_opacity(Window),
    io:format(user, "window opacity: ~p~n", [Opacity]),

    ok = glfw:set_window_opacity(Window, 0.5),
    % XXX: Check if the opacity was actually updated.

    ok = glfw:iconify_window(Window),
    ok = glfw:restore_window(Window),
    ok = glfw:maximize_window(Window),
    ok = glfw:show_window(Window),
    ok = glfw:hide_window(Window),
    ok = glfw:focus_window(Window),
    ok = glfw:request_window_attention(Window),

    Self = self(),
    undefined = glfw:window_position_handler(Window),
    ok = glfw:set_window_position_handler(Window, Self),
    Self = glfw:window_position_handler(Window),

    undefined = glfw:window_size_handler(Window),
    ok = glfw:set_window_size_handler(Window, Self),
    Self = glfw:window_size_handler(Window),

    undefined = glfw:window_close_handler(Window),
    ok = glfw:set_window_close_handler(Window, Self),
    Self = glfw:window_close_handler(Window),

    undefined = glfw:window_refresh_handler(Window),
    ok = glfw:set_window_refresh_handler(Window, Self),
    Self = glfw:window_refresh_handler(Window),

    undefined = glfw:window_focus_handler(Window),
    ok = glfw:set_window_focus_handler(Window, Self),
    Self = glfw:window_focus_handler(Window),

    undefined = glfw:window_iconify_handler(Window),
    ok = glfw:set_window_iconify_handler(Window, Self),
    Self = glfw:window_iconify_handler(Window),

    undefined = glfw:window_maximize_handler(Window),
    ok = glfw:set_window_maximize_handler(Window, Self),
    Self = glfw:window_maximize_handler(Window),

    undefined = glfw:window_content_scale_handler(Window),
    ok = glfw:set_window_content_scale_handler(Window, Self),
    Self = glfw:window_content_scale_handler(Window),

    undefined = glfw:framebuffer_size_handler(Window),
    ok = glfw:set_framebuffer_size_handler(Window, Self),
    Self = glfw:framebuffer_size_handler(Window),

    ok = glfw:poll_events(),
    ok = glfw:post_empty_event(),

    ok = glfw:destroy_window(Window),
    ?assertError(badarg, glfw:window_size(Window)),
    ?assertError(badarg, glfw:destroy_window(Window)),
    ok = glfw:terminate(),
    ?assertError(badarg, glfw:window_size(Window)),

    ok.

probe_egl_window_surface(Handle) ->
    case glfw:platform() of
        {ok, GlfwPlatform} ->
            NativeDisplay = glfw:display_egl_handle(),
            EglPlatform = egl_platform(GlfwPlatform),
            probe_egl_window_surface(Handle, EglPlatform, NativeDisplay);
        error ->
            io:format(user, "glfw platform failed~n", []),
            ok
    end.

egl_platform(wayland) -> wayland;
egl_platform(x11) -> x11;
egl_platform(win32) -> angle;
egl_platform(cocoa) -> angle;
egl_platform(_) -> undefined.

probe_egl_window_surface(_Handle, undefined, _NativeDisplay) ->
    ok;
probe_egl_window_surface(_Handle, _EglPlatform, error) ->
    io:format(user, "glfw display_egl_handle failed~n", []),
    ok;
probe_egl_window_surface(Handle, EglPlatform, NativeDisplay) ->
    Display = egl:get_platform_display(EglPlatform, NativeDisplay, []),
    case Display of
        no_display ->
            io:format(user, "egl get_platform_display ~p failed~n", [EglPlatform]),
            require_wayland_or_x11_surface(EglPlatform);
        _ ->
            case egl:initialize(Display) of
                {ok, _} ->
                    _ = egl:bind_api(opengl_api),
                    case egl:choose_config(Display, [
                        {surface_type, [window_bit]},
                        {renderable_type, [opengl_bit]}
                    ]) of
                        {ok, [Config | _]} ->
                            case egl:create_window_surface(Display, Config, Handle, []) of
                                {ok, Surface} ->
                                    ok = egl:destroy_surface(Display, Surface),
                                    ok;
                                Other ->
                                    io:format(
                                        user,
                                        "egl create_window_surface ~p error ~p~n",
                                        [Other, egl:get_error()]
                                    ),
                                    require_wayland_or_x11_surface(EglPlatform)
                            end;
                        Other ->
                            io:format(user, "egl choose_config ~p~n", [Other]),
                            require_wayland_or_x11_surface(EglPlatform)
                    end;
                Other ->
                    io:format(user, "egl initialize ~p error ~p~n",
                        [Other, egl:get_error()]),
                    require_wayland_or_x11_surface(EglPlatform)
            end
    end.

require_wayland_or_x11_surface(wayland) ->
    erlang:error(egl_window_surface_failed);
require_wayland_or_x11_surface(x11) ->
    erlang:error(egl_window_surface_failed);
require_wayland_or_x11_surface(_) ->
    ok.
