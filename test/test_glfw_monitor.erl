%%
%% A GLFW demo for testing monitor queries and connect/disconnect events.
%%
%% It prints connected monitors (read-only, including the current gamma ramp)
%% and then waits for monitor events. It never writes gamma.
%%
%% Ctrl+C to exit.
%%
-module(test_glfw_monitor).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

run() ->
    io:format("GLFW Monitor - Test Demo~n"),
    glfw:init(),
    test_error_handler:setup(),

    print_all_monitors(),

    glfw:monitor_set_handler(spawn(fun monitor_event_handler/0)),
    io:format("Waiting for monitor events... (press Ctrl+C to exit)~n"),
    poll_events().

print_all_monitors() ->
    Monitors = glfw:monitors(),
    Primary = glfw:primary_monitor(),
    io:format("connected monitors: ~p~n", [length(Monitors)]),
    io:format("primary monitor: ~p~n", [Primary]),
    lists:foreach(
        fun(Monitor) ->
            io:format("~n"),
            case Monitor =:= Primary of
                true ->
                    io:format("monitor ~p (primary)~n", [Monitor]);
                false ->
                    io:format("monitor ~p~n", [Monitor])
            end,
            display_monitor_info(Monitor)
        end,
        Monitors
    ),
    io:format("~n").

display_monitor_info(Monitor) ->
    io:format("  name:           ~p~n", [glfw:monitor_name(Monitor)]),
    io:format("  position:       ~p~n", [glfw:monitor_position(Monitor)]),
    io:format("  work area:      ~p~n", [glfw:monitor_work_area(Monitor)]),
    io:format("  physical size:  ~p~n", [glfw:monitor_physical_size(Monitor)]),
    io:format("  content scale:  ~p~n", [glfw:monitor_content_scale(Monitor)]),
    print_video_mode("  video mode:    ", glfw:video_mode(Monitor)),
    VideoModes = glfw:video_modes(Monitor),
    io:format("  video modes (~p):~n", [length(VideoModes)]),
    lists:foreach(
        fun(Mode) -> print_video_mode("    ", Mode) end,
        VideoModes
    ),
    case glfw:gamma_ramp(Monitor) of
        undefined ->
            io:format("  gamma ramp:     undefined (read-only)~n");
        #glfw_gamma_ramp{red = Red, green = Green, blue = Blue} ->
            io:format(
                "  gamma ramp:     red=~p green=~p blue=~p (read-only)~n",
                [length(Red), length(Green), length(Blue)]
            )
    end.

print_video_mode(Prefix, #glfw_video_mode{
    width = Width,
    height = Height,
    red_bits = RedBits,
    green_bits = GreenBits,
    blue_bits = BlueBits,
    refresh_rate = RefreshRate
}) ->
    io:format(
        "~s~px~p ~p/~p/~p @ ~pHz~n",
        [Prefix, Width, Height, RedBits, GreenBits, BlueBits, RefreshRate]
    ).

monitor_event_handler() ->
    receive
        #glfw_monitor{monitor = Monitor, event = connected} ->
            io:format("~nmonitor connected: ~p~n", [Monitor]),
            display_monitor_info(Monitor),
            monitor_event_handler();
        #glfw_monitor{monitor = Monitor, event = disconnected} ->
            io:format("~nmonitor disconnected: ~p~n", [Monitor]),
            monitor_event_handler();
        Event ->
            io:format("unknown monitor event: ~p~n", [Event]),
            monitor_event_handler()
    end.

poll_events() ->
    ok = glfw:poll_events(),
    timer:sleep(100),
    poll_events().
