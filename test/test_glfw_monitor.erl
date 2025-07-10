%%
%% A GLFW demo for testing monitor-related functionalities.
%%
%% To be written.
%%
-module(test_glfw_monitor).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").


run() ->
    io:format("GLFW Monitor Test Demo~n"),
    glfw:init(),
    test_error_handler:setup(),

    % Monitors = glfw:monitors(),
    % io:format("Available Monitors: ~p~n", [length(Monitors)]),

    Monitor = case glfw:primary_monitor() of
        undefined ->
            io:format("No primary monitor found.~n"),
            erlang:halt(0);
        Monitor_ ->
            io:format("Primary Monitor: ~p~n", [Monitor_]),
            display_monitor_info(Monitor_)
    end,

    % VideosModes = glfw:video_modes(Monitor),
    % io:format("Available Video Modes: ~p~n", [length(VideosModes)]),
    
    loop(Monitor).

loop(Monitor) ->
    Count = 0,

    io:format("~n"),
    io:format("Enter a command ~n"),

    % Prompt =  lists:flatten(io_lib:format("~p> ", [Count])),
    Command = io:get_line("yo"),

    case Command of
        "1\n" ->
            io:format("You chose to do this~n"),
            loop(Monitor);
        eof ->
            io:format("An unknown error occurred; exiting...~n"),
            exit(normal);
        W ->
            io:format("messg: ~p~n", [W]),
            io:format("Unknown command~n")
    end.

print_usage_lines() ->
    io:format("Commands:~n"),
    io:format("~n"),
    io:format("1) Do this~n"),
    io:format("2) Do that~n"),
    io:format("~n"),
    io:format("Type 'exit' to quit.~n~n").

display_monitor_info(Monitor) ->
    Name = glfw:monitor_name(Monitor),
    io:format("Monitor Name: ~p~n", [Name]),

    Position = glfw:monitor_position(Monitor),
    io:format("Monitor Position: ~p~n", [Position]),
    WorkArea = glfw:monitor_work_area(Monitor),
    io:format("Monitor Work Area: ~p~n", [WorkArea]),
    PhysicalSize = glfw:monitor_physical_size(Monitor),
    io:format("Monitor Physical Size: ~p~n", [PhysicalSize]),
    ContentScale = glfw:monitor_content_scale(Monitor),
    io:format("Monitor Content Scale: ~p~n", [ContentScale]),

    #glfw_video_mode{
        width = Width,
        height = Height,
        red_bits = RedBits,
        green_bits = GreenBits,
        blue_bits = BlueBits,
        refresh_rate = RefreshRate
    } = glfw:video_mode(Monitor),
    io:format("Monitor Video Mode: ~n"),
    io:format("  Width: ~p~n", [Width]),
    io:format("  Height: ~p~n", [Height]),
    io:format("  Red Bits: ~p~n", [RedBits]),
    io:format("  Green Bits: ~p~n", [GreenBits]),
    io:format("  Blue Bits: ~p~n", [BlueBits]),
    io:format("  Refresh Rate: ~p~n", [RefreshRate]),

    #glfw_gamma_ramp{
        red = RedRamp,
        green = GreenRamp,
        blue = BlueRamp
    } = glfw:gamma_ramp(Monitor),
    io:format("Monitor Gamma Ramp: ~n"),
    io:format("  Red: ~p~n", [RedRamp]),
    io:format("  Green: ~p~n", [GreenRamp]),
    io:format("  Blue: ~p~n", [BlueRamp]),

    ok.
