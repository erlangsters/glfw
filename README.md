# GLFW binding for the BEAM

> [!WARNING]
> :construction: It's still in development and a first beta version will be
> available shortly. The master branch will be rewound!

> [!IMPORTANT]
> This binding will not work on macOS.

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-glfw-%23a90432)](https://github.com/erlangsters/glfw)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/glfw)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/glfw/workflow.yml)](https://github.com/erlangsters/glfw/actions/workflows/workflow.yml)
[![Documentation Link](https://img.shields.io/badge/documentation-available-yellow)](http://erlangsters.github.io/glfw/)

A binding of [GFLW](https://www.glfw.org/) version 3.4 for the Erlang and
Elixir programming language. It's designed to work exclusively with the EGL 1.5
[binding](https://github.com/erlangsters/egl-1.5) and, indirectly, with one of
the OpenGL [bindings](https://github.com/orgs/erlangsters/repositories?type=all&q=opengl-).

```erlang
glfw:init().
{ok, Window} = glfw:create_window(640, 480, "Hello, World!").
```

It's a a thread-safe and idiomatic binding that is not generated. It's tested
to work flawlessly on Linux, macOs and Windows.

> For very good reasons, this binding has its limitations and will remain this
> way. For instance, it does not expose Vulkan-related features, and it limits
> you to contextless windows.

Written by the Erlangsters [community](https://about.erlangsters.org/) and
released under the MIT [license](/https://opensource.org/license/mit).

XXX: Implement child windows.

XXX: Verify/clarify  how glfwTerminate() could corrupt the BEAM by invalidating
     pointer to exisitng resource (cursor, window, etc).

XXX: What happens if monitor resource sent in the monitor callback, then later
     another retrieve with monitors/0. Should they be shared ?

XXX: Verify it builds on earlier version of GLFW (version 3.3 on debian 12 for instance).

XXX: Should time-related functions be implemented

- glfwGetTimerFrequency
- glfwGetTimerValue
- glfwGetTime
- glfwSetTime

It's for GLFW version 3.4, however it will compile fine with previous version.
Just make sure not to  use features if linking against earlier version. Use the
`glfw:version()`.


For more example, consult the test/ folder.


## Getting started

An example will speak louder than words.

```erlang
glfw:init().
{ok, Window} = glfw:create_window(800, 600, "Hello, GLFW!").

glfw:set_key_handler(Window, self()).
window_loop(Window).
```

> Notice how the concept of "callbacks" is replaced with "handlers". Instead of
> functions being called, we register a process which then receives the events
> as BEAM messages to process them asynchronously.

Here is how the main loop could look like.

```erlang
loop_window(Window) ->
case glfw:window_should_close(Window) of
     true ->
          glfw:terminate();
     false ->
          glfw:poll_events(),
          handle_events(Window),
          loop_window(Window)
end.
```

Here is how processing events could look like.


```erlang

handle_events(Window) ->
     receive
          #glfw_key{window=Window, key={X, Y}} ->
          glfw:window_should_close(Window, true),
          handle_events(Window);
     after 0 ->
          ok
     end.
```

Note about contextless window.

```erlang
WindowHandle = glfw:window_egl_handle(Window).
{ok, Surface} = egl:create_window_surface(Display, Config, WindowHandle, []).
```

To be written.

An effort to provide an adapted documentation was made, and therefore you can
rely on the binding's documentation.

## Contextless windows

To be written.

For more information, consult this [document](/docs/contextless-windows.md).

## Thread safety

To be written.

For more information, consult this [document](/docs/thread-safety.md).

## API mapping

This is a binding to a C API that has been slightly adapted to feel more
natural when used in Erlang and Elixir.

> Your existing GLFW knowledge still applies.

The changes are minor and shouldn’t cause any issues. With the API reference,
you should have everything you need.

If you run into difficulties, check the API mapping document for details on how
functions and types are mapped.

Additionally, the test suite in this repository serves as a useful reference
for practical examples of GLFW functions in action.

## Test demos

It's hard to put GLFW under automated unit tests since it's heavily stateful
and requires a graphical environment which is hard to emulate in Github
runners.

However, this binding comes with a limited set of unit tests that should
already indicate a great deal whether GLFW works on your system.

- `rebar3 eunit -m glfw_test` - Test the initialization-related functions of the API
- `rebar3 eunit -m glfw_monitor_test` - Test the monitor-related functions of the API

Additionally, it comes with interactive mini "test demos" that can be used to
quickly test various part of the binding on your system.

- `rebar3 as test shell --eval 'test_glfw_joystick:run().'` - To test Joystick/Gamepad
- `rebar3 as test shell --eval 'test_glfw_joystick:run().'` - To test Joystick/Gamepad
- `rebar3 as test shell --eval 'test_glfw_joystick:run().'` - To test Joystick/Gamepad
- `rebar3 as test shell --eval 'test_glfw_joystick:run().'` - To test Joystick/Gamepad
