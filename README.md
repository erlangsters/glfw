# GLFW binding for the BEAM

:construction: It's still in development and a first beta version will be
available shortly. The master branch will be rewound!

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-glfw-%23a90432)](https://github.com/erlangsters/glfw)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/glfw)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/glfw/workflow.yml)](https://github.com/erlangsters/glfw/actions/workflows/workflow.yml)
[![Documentation Link](https://img.shields.io/badge/documentation-available-yellow)](http://erlangsters.github.io/glfw/)

A binding of [GFLW](https://www.glfw.org/) version 3.4 for the Erlang and 
Elixir programming language. It's designed to work exclusively with the EGL 1.5
[binding](https://github.com/erlangsters/egl-1.5) and, indirectly, one of the
OpenGL [bindings](https://github.com/orgs/erlangsters/repositories?type=all&q=opengl-).

```erlang
glfw:init().
{ok, Window} = glfw:create_window(640, 480, "Hello, World!").
```

It's a manually written (not generated) binding which is tested to work on
Linux, macOs and Windows.

> For very good reasons, this binding has its limitations and will remain this
> way. For instance, it does not expose Vulkan-related features, and it limits
> you to contextless windows.

Written by the Erlangsters [community](https://about.erlangsters.org/) and
released under the MIT [license](/https://opensource.org/license/mit).

XXX: Implement child windows.

## Quick preview

To be written.

```

```

To be written.

## Getting started

It's a manually written (not generated) binding of the GLFW library to allow
the development of desktop applications with the EGL and OpenGL bindings.
Working with those libraries in C has thread-safety implications which are
nicely solved by the bindings-level (so you do not have to deal with them in
your BEAM-based applications). Consequently, all those bindings are designed to
work together, exclusively (because of inter-dependencies).

As much as possible, the API of the binding closely follow the API of the C
library it's wrapping. For instance, the GLFW must be initialized and can be
terminated just like in C.

```erlang
glfw:init_hint(this, that).
glfw:init().
% Do thing.
glfw:terminate().
```

Some adjustments here and where were made to make it more natural (or to align
with the existing convention used by all Erlangsters libraries and frameworks).
Notable examples are:

```erlang
last_error/set_error_handler.
no_pointer_user_data.

A monitor prefix was added!
glfwGetVideoModes
glfwGetVideoMode
glfwSetGamma
glfwGetGammaRamp
glfwSetGammaRamp


```





In C, when a pointer on some object is returend, in it's returned as a
reference. You still need to manually manage the lifetime of those. For instnace
if

```erlang
{ok, Window} = glfw:create_window(640, 480, "Hello, World!")
% Later...
ok = glfw:destroy_window(Window).
% Do not use the Window variable again...
```

About the versioning scheme, it closely follow the scheme of the upstream
library. If minor revision is made, the.
Until it reaches stable version, 0.0.1 will be used

An effort to provide an adapted documentation was made, and therefore you can
rely on the binding's documentation.

## Known Limitations

GLFW is a library to create windows with an underlying OpenGL context. However,
OpenGL contexts also means thread-safety (at the C level) which cannot be
achieved in pure Erlang or Elixir.

Fortunately, the EGL binding and its OpenGL bindings have solved this problem.
But since GLFW windows owns the OpenGL context, it's hard to really expose all
the GLFW features. As a consequences, this binding is incomplete and limits you
to the creation of contextless windows.

```erlang
{ok, Window} = glfw:create_window(640, 480, "Hello, World!")
```

All windows created by this binding are contextless, but it's fine, the EGL
binding lets you create EGL contexts with your desired parameters equally well.
Later, you will just have to manually create an EGL surface from the EGL window
handle.

```erlang
{ok, WindowHandle} = glfw:window_egl_handle(Window)
{ok, Surface} = egl:create_window_surface(Display, Config, WindowHandle, []),
```

Equipped with all the EGL instances (display, context, and surface), you do not
need a function like `glfw:window_swap_buffers/1` and all the context-related
functions (which are indeed not provided).

> Observe how the `glfw:window_egl_handle/1` is what ties this GLFW binding
to the EGL binding. It returns a compatible EGL window handle that will be accepted by
the EGL binding.

Next is about the event loop. Again, GLFW is not thread-safe and most of its
functions are designed to run from the main thread. Meaning that a 1 to 1 API
is not even desirable.

```erlang

```


```erlang
```

In Erlang (or Elixir), you do not have to worry about all those functions are
thread-safe and can be called from just anywhere.


## Using it in your project

With the **Rebar3** build system, add the following to the `rebar.config` file
of your project.

```
{deps, [
  {egl, {git, "https://github.com/erlangsters/egl-1.5.git", {tag, "master"}}},
  {glfw, {git, "https://github.com/erlangsters/glfw.git", {tag, "master"}}}
]}.
```

If you happen to use the **Erlang.mk** build system, then add the following to
your Makefile.

```
BUILD_DEPS = egl glfw
dep_egl = git https://github.com/erlangsters/egl-1.5 master
dep_glfw = git https://github.com/erlangsters/glfw master
```

In practice, you want to replace the branch "master" with a specific "tag" to
avoid breaking your project if incompatible changes are made.
