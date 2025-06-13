# Contextless Windows

GLFW is a library to create windows with an underlying **OpenGL context**
(unless explicitly disabled). However, OpenGL contexts are problematic at the
BEAM level because they are intrinsically linked to **OS threads**. It's the
reason why the OpenGL bindings and this binding itself have an internal
dependency on the EGL binding which solves the problem.

## Windows with an OpenGL context

Usually, GLFW is used to create both the window and the associated OpenGL
context.

```c
glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

GLFWwindow* window = glfwCreateWindow(640, 480, "My Window", NULL, NULL);

EGLDisplay display = glfwGetEGLDisplay();
EGLContext context = glfwGetEGLContext(window);
EGLSurface surface = glfwGetEGLSurface(window);
```

Notice that it's a feature. Convenience is what users look for in GLFW, and
that saves you from doing it manually.

However, this feature cannot exposed by this binding because the EGL binding
needs control over the OpenGL context to solve the threading issue stated at
the beginning of this document.

With that said, it really is not a big deal. What GLFW usually does for you,
you can do it yourself.

```c
EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
eglInitialize(display, NULL, NULL);

eglBindAPI(EGL_OPENGL_API);

EGLint configAttribs[] = {
    EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
    EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
    EGL_NONE
};

EGLConfig config;
EGLint numConfigs;
eglChooseConfig(display, configAttribs, &config, 1, &numConfigs);

EGLint contextAttribs[] = {
    EGL_CONTEXT_MAJOR_VERSION, 3,
    EGL_NONE
};
EGLContext context = eglCreateContext(display, config, EGL_NO_CONTEXT, contextAttribs);

glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
GLFWwindow* window = glfwCreateWindow(640, 480, "My Window", NULL, NULL);

// XXX
EGLNativeWindowType windowHandle = (EGLNativeWindowType)glfwGetX11Window(window);
EGLSurface surface = eglCreateWindowSurface(display, config, windowHandle, NULL);

eglMakeCurrent(display, surface, surface, context);
```

As a consequence, the GLFW binding limits you to the creation of contextless
windows.

## Contextless windows limitation

Now that we understand that we cannot create a window with an underlying OpenGL
context, let's see how it gets done manually in Erlang and Elixir.

```erlang
Display = egl:get_display(default_display).
{ok, {_, _}} = egl:initialize(Display).

egl:bind_api(opengl_api).

ConfigAttribs = [
    {surface_type, [window_bit]},
    {renderable_type, [opengl_bit]}
].
{ok, Configs} = egl:choose_config(Display, ConfigAttribs).

ContextAttribs = [{context_major_version, 3}].
{ok, Context} =
egl:create_context(Display, hd(Configs), no_context, ContextAttribs).

{ok, Window} = glfw:create_window(640, 480, "My Window"),

WindowHandle = glfw:window_egl_handle(Window).
{ok, Surface} = egl:create_window_surface(Display, Config, WindowHandle, []).
ok = egl:make_current(Display, Surface, Surface, Context).
```

> Observe how the `glfw:window_egl_handle/1` is what ties this GLFW binding
> to the EGL binding. It returns a compatible EGL window handle that will be
> accepted by the EGL binding.

Equipped with all the EGL instances (display, context, and surface), you do not
need a function like `glfw:window_swap_buffers/1` and all the context-related
functions.

```erlang
egl:swap_buffers(Display, Surface).
```

Note that all features related to "windows with a context" are not implemented,
obviously. As such, there are no functions like `glfw:get_egl_window/1` or
`glfw:get_egl_display/0`.