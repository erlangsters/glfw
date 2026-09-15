# GLFW binding for the BEAM

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-glfw-%23a90432)](https://github.com/erlangsters/glfw)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/glfw)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/glfw/build.yml)](https://github.com/erlangsters/glfw/actions/workflows/build.yml)
[![Documentation Link](https://img.shields.io/badge/documentation-available-yellow)](http://erlangsters.github.io/glfw/)

> [!WARNING]
> :construction: It's still in development and a first beta version will be available shortly. The master branch will be rewound!

> [!IMPORTANT]
> This binding will not work on macOS.

A binding of [GLFW](https://www.glfw.org/) version 3.4 for the Erlang and Elixir programming language. It's designed to work exclusively with the EGL 1.5 [binding](https://github.com/erlangsters/egl-1.5) and, indirectly, with one of the OpenGL [bindings](https://github.com/orgs/erlangsters/repositories?type=all&q=opengl-).

```erlang
glfw:init().
{ok, Window} = glfw:create_window(640, 480, "Hello, World!").
```

It's a thread-safe and idiomatic binding that is not generated. Linux is the runtime-verified platform. macOS and Windows compile in CI; they are not a first-release runtime claim.

For very good reasons, this binding has its limitations and will remain this way. For instance, it does not expose Vulkan-related features, and it limits you to contextless windows.

Written by the Erlangsters [community](https://about.erlangsters.org/) and released under the MIT [license](https://opensource.org/license/mit).

## Getting started

An example will speak louder than words.

```erlang
-include_lib("glfw/include/glfw.hrl").

glfw:init().
{ok, Window} = glfw:create_window(800, 600, "Hello, GLFW!").
glfw:set_key_handler(Window, self()).
window_loop(Window).
```

Callbacks are replaced with handlers. You register a process, and it receives events as BEAM messages.

Here is how the main loop could look.

```erlang
window_loop(Window) ->
    case glfw:window_should_close(Window) of
        true ->
            glfw:terminate();
        false ->
            glfw:poll_events(),
            handle_events(Window),
            window_loop(Window)
    end.
```

Here is how processing events could look.

```erlang
handle_events(Window) ->
    receive
        #glfw_key{window = Window, key = key_escape, action = press} ->
            glfw:set_window_should_close(Window, true),
            handle_events(Window)
    after 0 ->
        ok
    end.
```

The contract is GLFW 3.4. It may compile against an earlier GLFW, but do not use 3.4-only features in that case. `glfw:version/0` reports the linked library.

The test demos in this repository are the practical examples.

## Contextless windows

Windows are always created without an OpenGL context. EGL owns the context and the window surface. `window_egl_handle/1` is the bridge:

```erlang
WindowHandle = glfw:window_egl_handle(Window).
{ok, Surface} = egl:create_window_surface(Display, Config, WindowHandle, []).
```

For more information, consult this [document](docs/contextless-windows.md).

## Thread safety

The binding is thread-safe at the BEAM level. GLFW itself is not. Calls run on a dedicated OS thread, and events arrive as handler messages rather than C callbacks.

For more information, consult this [document](docs/thread-safety.md).

## API mapping

This is a binding to a C API that has been slightly adapted to feel more natural when used in Erlang and Elixir.

> Your existing GLFW knowledge still applies.

The changes are minor. With the API reference, you should have everything you need. If a GLFW feature does not look the way you expect, check the [API mapping](docs/api-mapping.md).

## Test demos

It's hard to put GLFW under automated unit tests since it's heavily stateful and requires a graphical environment which is hard to emulate in GitHub runners.

However, this binding comes with a limited set of unit tests that should already indicate a great deal whether GLFW works on your system.

- `rebar3 eunit -m glfw_test` — initialization
- `rebar3 eunit -m glfw_window_test` — window queries and handlers
- `rebar3 eunit -m glfw_cursor_test` — cursor objects
- `rebar3 eunit -m glfw_input_test` — input modes and keys
- `rebar3 eunit -m glfw_monitor_test` — read-only monitor queries

Monitor eunit does not write gamma ramps. Do not call `glfw:set_gamma/2` or `glfw:set_gamma_ramp/2` unless you intend to change the OS display calibration.

Interactive mini "test demos" can be used to quickly test parts of the binding on your system:

- `rebar3 as test shell --eval 'test_glfw_event:run().'` — window and input events
- `rebar3 as test shell --eval 'test_glfw_window:run().'` — window state and mutation
- `rebar3 as test shell --eval 'test_glfw_monitor:run().'` — read-only monitors
- `rebar3 as test shell --eval 'test_glfw_input:run().'` — input modes, clipboard, drop
- `rebar3 as test shell --eval 'test_glfw_joystick:run().'` — joystick and gamepad

## Installing the library

This repository is unreleased. The `master` branch is development-only and will be rewound.

To use glfw in a rebar3 project, add it to your rebar.config.

```erlang
{deps, [
  {glfw, {git, "https://github.com/erlangsters/glfw.git", {branch, "master"}}}
]}.
```

Native compilation needs a C compiler, CMake, GLFW 3.4, and EGL. On Linux the GitHub [workflow](.github/workflows/build.yml) installs `cmake`, `libegl-dev`, `libglfw3-dev`, and `libwayland-dev`.
