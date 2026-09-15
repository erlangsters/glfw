# Thread safety

GLFW is not thread-safe. This binding is thread-safe at the BEAM level: you can call any exported `glfw` function from any process.

Do not confuse that with the OpenGL bindings. Those are not thread-safe at the BEAM level. An OpenGL context is bound to an OS thread, which is why this graphics stack uses EGL to create and activate contexts.

## Why GLFW is not thread-safe

GLFW unifies several window systems behind one C API. On many platforms, window and event functions must run on a single thread, often the thread that called `glfwInit`. Code that happens to work on one platform can fail on another if those calls are made from the wrong OS thread.

GLFW therefore documents almost every function as main-thread only. If all GLFW calls run on that one thread, GLFW guarantees they work.

## How the binding solves it

The NIF starts a dedicated OS thread when it loads, the command executor. `init/0` runs there, and so do the functions GLFW marks as main-thread only.

When GLFW documents a function as safe to call from any thread, the binding calls it on the BEAM scheduler thread that entered the NIF. `version/0`, `version_string/0`, `get_error/0`, `platform/0`, and `platform_supported/1` take that path.

You never see the executor. There is no public API for it. From Erlang, every call looks like an ordinary function.

## From callbacks to handlers

GLFW delivers events by invoking C callbacks during `glfwPollEvents`. That model is built around the OS thread that polls.

This binding does not expose those callbacks. You register a handler pid. `poll_events/0` still runs on the executor, GLFW still fires its C callbacks there, and the binding sends Erlang records to the registered process.

```erlang
glfw:set_key_handler(Window, self()).
glfw:poll_events(),
receive
    #glfw_key{window = Window, key = Key, action = Action} ->
        {Key, Action}
after 0 ->
    no_event
end.
```

Setters do not return the previous handler. Pass `undefined` to unregister. The matching getter (`key_handler/1`, and so on) returns the current pid or `undefined`.

## Why `wait_events` is not implemented

`glfwWaitEvents` and `glfwWaitEventsTimeout` would block the executor until a window-system event arrives. That fights both the handler model and the rest of the binding, which needs that thread to run GLFW calls.

Use `poll_events/0` and a BEAM `receive` instead. `post_empty_event/0` still exists; it posts an empty event to the GLFW queue so a waiter would return, but this binding has no waiter. Ordinary event processing is `poll_events/0`.
