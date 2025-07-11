%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw_monitor_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("glfw/include/glfw.hrl").

glfw_monitor_test() ->
    [] = glfw:monitors(),
    undefined = glfw:primary_monitor(),

    true = glfw:init(),

    Monitors = glfw:monitors(),
    ?assert(erlang:is_list(Monitors)),

    Monitor = glfw:primary_monitor(),
    ?assert(erlang:is_reference(Monitor)),

    {X1, Y1} = glfw:monitor_position(Monitor),
    io:format(user, "monitor position (x: ~p, y)~p~n", [X1, Y1]),
    ?assert(erlang:is_integer(X1)),
    ?assert(erlang:is_integer(Y1)),

    {X2, Y2, Width2, Height2} = glfw:monitor_work_area(Monitor),
    io:format(user, "monitor work area (x: ~p, y: ~p, width: ~p, height: ~p)~n", [X2, Y2, Width2, Height2]),
    ?assert(erlang:is_integer(X2)),
    ?assert(erlang:is_integer(Y2)),
    ?assert(erlang:is_integer(Width2)),
    ?assert(erlang:is_integer(Height2)),

    {Width3, Height3} = glfw:monitor_physical_size(Monitor),
    io:format(user, "monitor physical size (width: ~p, height: ~p)~n", [Width3, Height3]),
    ?assert(erlang:is_integer(Width3)),
    ?assert(erlang:is_integer(Height3)),

    {X4, Y4} = glfw:monitor_content_scale(Monitor),
    io:format(user, "monitor content scale (x: ~p, y: ~p)~n", [X4, Y4]),
    ?assert(erlang:is_float(X4)),
    ?assert(erlang:is_float(Y4)),

    Name = glfw:monitor_name(Monitor),
    io:format(user, "monitor name: ~p~n", [Name]),
    ?assert(erlang:is_list(Name)),

    VideoModes = glfw:video_modes(Monitor),
    lists:foreach(fun print_video_mode/1, VideoModes),

    VideoMode = glfw:video_mode(Monitor),
    print_video_mode(VideoMode),

    ok = glfw:set_gamma(Monitor, 1.0),

    GammaRamp = glfw:gamma_ramp(Monitor),
    print_gamma_ramp(GammaRamp),

    ok = glfw:set_gamma_ramp(Monitor, GammaRamp),

    ok.

print_video_mode(#glfw_video_mode{
    width = Width,
    height = Height,
    red_bits = RedBits,
    green_bits = GreenBits,
    blue_bits = BlueBits,
    refresh_rate = RefreshRate
}) ->
    io:format(user, "video mode (width: ~p, height: ~p, red bits: ~p, green bits: ~p, blue bits: ~p, refresh rate: ~p)~n", [Width, Height, RedBits, GreenBits, BlueBits, RefreshRate]).

print_gamma_ramp(#glfw_gamma_ramp{
    red = Red,
    green = Green,
    blue = Blue
}) ->
    io:format(
        user,
        "gamma ramp (red: ~p, green: ~p, blue: ~p)~n",
        [erlang:length(Red), erlang:length(Green), erlang:length(Blue)]
    ).
