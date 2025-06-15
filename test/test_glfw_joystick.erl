-module(test_glfw_joystick).
-export([run/0]).

-include_lib("glfw/include/glfw.hrl").

run() ->
    io:format("GLFW Joystick/Gamepad Test Demo~n"),
    glfw:init(),

    Joysticks = [
        joystick_1,
        joystick_2,
        joystick_3,
        joystick_4,
        joystick_5,
        joystick_6,
        joystick_7,
        joystick_8,
        joystick_9,
        joystick_10,
        joystick_11,
        joystick_12,
        joystick_13,
        joystick_14,
        joystick_15,
        joystick_16
    ],
    lists:foreach(fun(Joystick) ->
        case glfw:joystick_present(Joystick) of
            true ->
                io:format("Joystick '~p' is present~n", [Joystick]),
                display_joystick_info(Joystick);
            false ->
                io:format("Joystick '~p' is not present~n", [Joystick]),
                not_present = glfw:joystick_axes(Joystick),
                not_present = glfw:joystick_buttons(Joystick),
                not_present = glfw:joystick_hats(Joystick),
                not_present = glfw:joystick_guid(Joystick),
                error = glfw:gamepad_name(Joystick),
                error = glfw:gamepad_state(Joystick)
        end
    end, Joysticks),

    io:format("Waiting for joystick events... (press Ctrl+C to exit)~n"),
    glfw:set_joystick_handler(spawn(fun joystick_event_handler/0)),

    % For events to be received, we need to poll events continuously.
    poll_events().

poll_events() ->
    ok = glfw:poll_events(),
    timer:sleep(100),
    poll_events().

display_joystick_info(Joystick) ->
    Name = glfw:joystick_name(Joystick),
    io:format("Joystick Name: ~p~n", [Name]),
    GUID = glfw:joystick_guid(Joystick),
    io:format("Joystick GUID: ~p~n", [GUID]),
    Axes = glfw:joystick_axes(Joystick),
    io:format("Joystick Axes: ~p~n", [Axes]),
    Buttons = glfw:joystick_buttons(Joystick),
    io:format("Joystick Buttons: ~p~n", [Buttons]),
    Hats = glfw:joystick_hats(Joystick),
    io:format("Joystick Hats: ~p~n", [Hats]),

    case glfw:joystick_is_gamepad(Joystick) of
        true ->
            io:format("Joystick '~p' is a gamepad.~n", [Joystick]),
            {ok, Name1} = glfw:gamepad_name(Joystick),
            io:format("Gamepad Name: ~p~n", [Name1]),
            {ok, AxesState, ButtonsState} = glfw:gamepad_state(Joystick),
            io:format("Gamepad Axes State: ~p~n", [AxesState]),
            io:format("Gamepad Buttons State: ~p~n", [ButtonsState]);
        false ->
            io:format("Joystick '~p' is not a gamepad.~n", [Joystick]),
            error = glfw:gamepad_name(Joystick),
            error = glfw:gamepad_state(Joystick)
    end,

    ok.

joystick_event_handler() ->
    receive
        #glfw_joystick{joystick=Joystick, event=connected} ->
            io:format("Joystick '~p' is connected.~n", [Joystick]),
            display_joystick_info(Joystick),
            joystick_event_handler();
        #glfw_joystick{joystick=Joystick, event=disconnected} ->
            io:format("Joystick '~p' is disconnected.~n", [Joystick]),
            joystick_event_handler()
    end.
