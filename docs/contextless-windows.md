# Contextless windows

GLFW is usually used to create a window together with an OpenGL context. OpenGL contexts are bound to OS threads, and the BEAM runs Erlang code on arbitrary scheduler threads. That is why this graphics stack gives context and surface creation to the EGL binding, and why this GLFW binding only creates contextless windows.

## Windows with an OpenGL context

A typical C GLFW program creates both the window and the context:

```c
glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

GLFWwindow* window = glfwCreateWindow(640, 480, "My Window", NULL, NULL);
```

GLFW can then hand back EGL objects for that window (`glfwGetEGLDisplay`, `glfwGetEGLContext`, `glfwGetEGLSurface`). Convenience is the point of that path.

This binding cannot expose it. The EGL binding must own the OpenGL context so it can solve the threading issue above. `create_window/3` always forces `GLFW_CLIENT_API = GLFW_NO_API` after your window hints. There is no `client_api` hint, and there are no GLFW context, swap, or `glfwGetEGL*` functions.

That is not a large loss. What GLFW would do for you, you do with EGL:

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
{ok, Surface} = egl:create_window_surface(Display, hd(Configs), WindowHandle, []).
ok = egl:make_current(Display, Surface, Surface, Context).
```

`window_egl_handle/1` is the only native-handle bridge. It returns a handle `egl:create_window_surface/4` accepts. Destroy the EGL surface before destroying the window; the handle becomes invalid when the window is destroyed.

Swap and current-context operations stay on EGL:

```erlang
egl:swap_buffers(Display, Surface).
egl:swap_interval(Display, 1).
```

## Wayland

`window_egl_handle/1` follows the platform selected at `init/0`. On Wayland it builds a `wl_egl_window` from `glfwGetWaylandWindow` and resizes it from the framebuffer-size callback. On X11, Win32, and Cocoa it is the platform window handle.

Creating an EGL window surface still goes through `egl:get_display(default_display)`, which uses `eglGetDisplay(EGL_DEFAULT_DISPLAY)`. On a Wayland session that handle is accepted by GLFW, then `egl:create_window_surface/4` can fail with `bad_alloc`. That is an `egl-1.5` follow-up (`eglGetPlatformDisplay` is not implemented there). Until EGL can open a Wayland display, apps can still force X11/Xwayland:

    env -u WAYLAND_DISPLAY DISPLAY="${DISPLAY:-:0}" ...
