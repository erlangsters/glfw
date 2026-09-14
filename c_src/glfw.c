//
// Copyright (c) 2026, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project directory.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
//
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#if defined(_WIN32)
    #include <windows.h>
#else
    #include <dlfcn.h>
#endif
#include <erl_nif.h>
#include <EGL/egl.h>
#include <GLFW/glfw3.h>
#if defined(_WIN32)
    #define GLFW_EXPOSE_NATIVE_WIN32
#elif defined(__APPLE__)
    #define GLFW_EXPOSE_NATIVE_COCOA
#else
    #if defined(BEAM_GLFW_HAS_NATIVE_X11)
        #define GLFW_EXPOSE_NATIVE_X11
    #endif
    #if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
        #define GLFW_EXPOSE_NATIVE_WAYLAND
    #endif
#endif
#include <GLFW/glfw3native.h>
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    #include <wayland-egl-core.h>
#endif
#include "command_executor.h"

#ifndef GLFW_PLATFORM_WIN32
#define GLFW_PLATFORM_WIN32   0x00060001
#define GLFW_PLATFORM_COCOA   0x00060002
#define GLFW_PLATFORM_WAYLAND 0x00060003
#define GLFW_PLATFORM_X11     0x00060004
#define GLFW_PLATFORM_NULL    0x00060005
#endif

typedef ErlNifResourceType* (*get_egl_window_resource_type_fn)(ErlNifEnv*);
get_egl_window_resource_type_fn get_egl_window_resource_type = NULL;

static void* egl_nif_lib_handle = NULL;
static ErlNifResourceType* egl_window_resource_type;

static CommandExecutor command_executor;
static int beam_glfw_initialized = 0;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static ERL_NIF_TERM atom_press;
static ERL_NIF_TERM atom_release;
static ERL_NIF_TERM atom_repeat;

static ERL_NIF_TERM atom_no_monitor;
static ERL_NIF_TERM atom_no_window;

static ERL_NIF_TERM atom_glfw_error;

static ERL_NIF_TERM atom_glfw_monitor;

static ERL_NIF_TERM atom_glfw_video_mode;
static ERL_NIF_TERM atom_glfw_gamma_ramp;
static ERL_NIF_TERM atom_glfw_window_position;
static ERL_NIF_TERM atom_glfw_window_size;
static ERL_NIF_TERM atom_glfw_window_close;
static ERL_NIF_TERM atom_glfw_window_refresh;
static ERL_NIF_TERM atom_glfw_window_focus;
static ERL_NIF_TERM atom_glfw_window_iconify;
static ERL_NIF_TERM atom_glfw_window_maximize;
static ERL_NIF_TERM atom_glfw_window_content_scale;
static ERL_NIF_TERM atom_glfw_framebuffer_size;

static ERL_NIF_TERM atom_glfw_key;
static ERL_NIF_TERM atom_glfw_char;
static ERL_NIF_TERM atom_glfw_char_mods;
static ERL_NIF_TERM atom_glfw_mouse_button;
static ERL_NIF_TERM atom_glfw_cursor_position;
static ERL_NIF_TERM atom_glfw_cursor_enter;
static ERL_NIF_TERM atom_glfw_scroll;
static ERL_NIF_TERM atom_glfw_drop;

static ERL_NIF_TERM atom_glfw_joystick;

static ERL_NIF_TERM atom_press;
static ERL_NIF_TERM atom_release;

static ERL_NIF_TERM atom_not_present;
static ERL_NIF_TERM atom_joysticks[GLFW_JOYSTICK_LAST + 1];
static ERL_NIF_TERM atom_connected;
static ERL_NIF_TERM atom_disconnected;

// Gamepad atoms.
static ERL_NIF_TERM atom_axe_left_x;
static ERL_NIF_TERM atom_axe_left_y;
static ERL_NIF_TERM atom_axe_right_x;
static ERL_NIF_TERM atom_axe_right_y;
static ERL_NIF_TERM atom_axe_left_trigger;
static ERL_NIF_TERM atom_axe_right_trigger;
static ERL_NIF_TERM atom_button_a;
static ERL_NIF_TERM atom_button_b;
static ERL_NIF_TERM atom_button_x;
static ERL_NIF_TERM atom_button_y;
static ERL_NIF_TERM atom_button_left_bumper;
static ERL_NIF_TERM atom_button_right_bumper;
static ERL_NIF_TERM atom_button_back;
static ERL_NIF_TERM atom_button_start;
static ERL_NIF_TERM atom_button_guide;
static ERL_NIF_TERM atom_button_left_thumb;
static ERL_NIF_TERM atom_button_right_thumb;
static ERL_NIF_TERM atom_button_dpad_up;
static ERL_NIF_TERM atom_button_dpad_right;
static ERL_NIF_TERM atom_button_dpad_down;
static ERL_NIF_TERM atom_button_dpad_left;

static ErlNifResourceType* glfw_monitor_resource_type = NULL;
static ErlNifResourceType* glfw_window_resource_type = NULL;
static ErlNifResourceType* glfw_cursor_resource_type = NULL;

static ErlNifEnv* glfw_error_handler_env = NULL;
static ErlNifEnv* glfw_monitor_handler_env = NULL;
static ErlNifEnv* glfw_joystick_handler_env = NULL;

typedef struct {
    int set;
    ErlNifPid pid;
} BeamHandler;

static BeamHandler glfw_error_handler;
static BeamHandler glfw_monitor_handler;
static BeamHandler glfw_joystick_handler;

typedef struct GLFWMonitorResource {
    GLFWmonitor* monitor;
    int alive;
    struct GLFWMonitorResource* intern_next;
} GLFWMonitorResource;

typedef struct GLFWCursorResource {
    GLFWcursor* cursor;
    int alive;
    struct GLFWCursorResource* live_next;
} GLFWCursorResource;

typedef struct GLFWWindowResource {
    ErlNifEnv* env;
    GLFWwindow* window;
    int alive;
    ERL_NIF_TERM window_term;
    BeamHandler window_position_handler;
    BeamHandler window_size_handler;
    BeamHandler window_close_handler;
    BeamHandler window_refresh_handler;
    BeamHandler window_focus_handler;
    BeamHandler window_iconify_handler;
    BeamHandler window_maximize_handler;
    BeamHandler window_content_scale_handler;
    BeamHandler framebuffer_size_handler;
    BeamHandler key_handler;
    BeamHandler char_handler;
    BeamHandler char_mods_handler;
    BeamHandler mouse_button_handler;
    BeamHandler cursor_position_handler;
    BeamHandler cursor_enter_handler;
    BeamHandler scroll_handler;
    BeamHandler drop_handler;
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    struct wl_egl_window* wayland_egl_window;
#endif
    struct GLFWWindowResource* live_next;
} GLFWWindowResource;

static GLFWMonitorResource* interned_monitors = NULL;
static GLFWWindowResource* live_windows = NULL;
static GLFWCursorResource* live_cursors = NULL;

static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
static void monitor_callback(GLFWmonitor* monitor, int event);

#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
static void beam_glfw_destroy_wayland_egl_window(GLFWWindowResource* window_resource)
{
    if (window_resource->wayland_egl_window != NULL) {
        wl_egl_window_destroy(window_resource->wayland_egl_window);
        window_resource->wayland_egl_window = NULL;
    }
}
#endif

ERL_NIF_TERM execute_command(
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[]
) {
    ERL_NIF_TERM result;
    command_executor_execute(
        &command_executor,
        function,
        env,
        argc,
        argv,
        &result
    );
    return result;
}

static void beam_handler_clear(BeamHandler* handler)
{
    handler->set = 0;
}

static int beam_handler_from_term(ErlNifEnv* env, ERL_NIF_TERM term, BeamHandler* handler)
{
    if (enif_is_identical(term, atom_undefined)) {
        handler->set = 0;
        return 1;
    }
    if (!enif_get_local_pid(env, term, &handler->pid)) {
        return 0;
    }
    handler->set = 1;
    return 1;
}

static ERL_NIF_TERM beam_handler_to_term(ErlNifEnv* env, const BeamHandler* handler)
{
    if (!handler->set) {
        return atom_undefined;
    }
    return enif_make_pid(env, &handler->pid);
}

static void beam_handler_send(ErlNifEnv* msg_env, const BeamHandler* handler, ERL_NIF_TERM message)
{
    if (!handler->set) {
        return;
    }
    enif_send(NULL, &handler->pid, msg_env, message);
}

static char* beam_alloc_utf8(ErlNifEnv* env, ERL_NIF_TERM term)
{
    unsigned length;
    if (!enif_get_list_length(env, term, &length)) {
        return NULL;
    }

    size_t capacity = (size_t)length * 4 + 1;
    char* buffer = malloc(capacity);
    if (buffer == NULL) {
        return NULL;
    }
    if (!enif_get_string(env, term, buffer, (unsigned)capacity, ERL_NIF_UTF8)) {
        free(buffer);
        return NULL;
    }
    return buffer;
}

static void beam_unlink_monitor(GLFWMonitorResource* resource)
{
    GLFWMonitorResource** slot = &interned_monitors;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->intern_next;
            resource->intern_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->intern_next;
    }
}

static void beam_poison_monitor(GLFWMonitorResource* resource)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    resource->monitor = NULL;
    beam_unlink_monitor(resource);
}

static ERL_NIF_TERM beam_intern_monitor(ErlNifEnv* env, GLFWmonitor* monitor)
{
    GLFWMonitorResource* it;
    for (it = interned_monitors; it != NULL; it = it->intern_next) {
        if (it->alive && it->monitor == monitor) {
            return enif_make_resource(env, it);
        }
    }

    GLFWMonitorResource* resource = enif_alloc_resource(
        glfw_monitor_resource_type,
        sizeof(GLFWMonitorResource)
    );
    resource->monitor = monitor;
    resource->alive = 1;
    resource->intern_next = interned_monitors;
    interned_monitors = resource;
    enif_keep_resource(resource);

    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static int beam_get_monitor(ErlNifEnv* env, ERL_NIF_TERM term, GLFWMonitorResource** out)
{
    GLFWMonitorResource* resource;
    if (!enif_get_resource(env, term, glfw_monitor_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->monitor == NULL) {
        return 0;
    }
    *out = resource;
    return 1;
}

static void beam_unlink_window(GLFWWindowResource* resource)
{
    GLFWWindowResource** slot = &live_windows;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->live_next;
            resource->live_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->live_next;
    }
}

static void beam_poison_window(GLFWWindowResource* resource, int destroy_native)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    beam_glfw_destroy_wayland_egl_window(resource);
#endif
    if (destroy_native && resource->window != NULL) {
        glfwDestroyWindow(resource->window);
    }
    resource->window = NULL;
    beam_unlink_window(resource);
}

static int beam_get_window(ErlNifEnv* env, ERL_NIF_TERM term, GLFWWindowResource** out)
{
    GLFWWindowResource* resource;
    if (!enif_get_resource(env, term, glfw_window_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->window == NULL) {
        return 0;
    }
    *out = resource;
    return 1;
}

static void beam_unlink_cursor(GLFWCursorResource* resource)
{
    GLFWCursorResource** slot = &live_cursors;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->live_next;
            resource->live_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->live_next;
    }
}

static void beam_poison_cursor(GLFWCursorResource* resource, int destroy_native)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    if (destroy_native && resource->cursor != NULL) {
        glfwDestroyCursor(resource->cursor);
    }
    resource->cursor = NULL;
    beam_unlink_cursor(resource);
}

static int beam_get_cursor(ErlNifEnv* env, ERL_NIF_TERM term, GLFWCursorResource** out)
{
    GLFWCursorResource* resource;
    if (!enif_get_resource(env, term, glfw_cursor_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->cursor == NULL) {
        return 0;
    }
    *out = resource;
    return 1;
}

static void beam_poison_all_resources(int destroy_native)
{
    while (live_windows != NULL) {
        beam_poison_window(live_windows, destroy_native);
    }
    while (live_cursors != NULL) {
        beam_poison_cursor(live_cursors, destroy_native);
    }
    while (interned_monitors != NULL) {
        beam_poison_monitor(interned_monitors);
    }
}

static void glfw_monitor_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    GLFWMonitorResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->monitor = NULL;
        beam_unlink_monitor(resource);
    }
}

static void glfw_window_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    GLFWWindowResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
        beam_glfw_destroy_wayland_egl_window(resource);
#endif
        resource->window = NULL;
        beam_unlink_window(resource);
    }
    if (resource->env != NULL) {
        enif_free_env(resource->env);
        resource->env = NULL;
    }
}

static void glfw_cursor_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    GLFWCursorResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->cursor = NULL;
        beam_unlink_cursor(resource);
    }
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    (void)priv_data;

    char beam_egl_nif_path[1024];
    if (!enif_get_string(env, arg, beam_egl_nif_path, sizeof(beam_egl_nif_path), ERL_NIF_LATIN1)) {
        fprintf(stderr, "failed to read EGL binding library path from argument\n");
        return -1;
    }

#if defined(_WIN32)
    HINSTANCE egl_nif_lib_handle = LoadLibrary(beam_egl_nif_path);
    if (!egl_nif_lib_handle) {
        printf("failed to load beam-egl.dll: %s\n", GetLastError());
        return -1;
    }

    get_egl_window_resource_type = (get_egl_window_resource_type_fn)GetProcAddress(egl_nif_lib_handle, "get_egl_window_resource_type");
    if (!get_egl_window_resource_type) {
        printf("failed to load symbol get_egl_window_resource_type: %s\n", GetLastError());
        FreeLibrary(egl_nif_lib_handle);
        return -1;
    }
#else
    egl_nif_lib_handle = dlopen(beam_egl_nif_path, RTLD_NOW | RTLD_GLOBAL);
    if (!egl_nif_lib_handle) {
        fprintf(stderr, "failed to load beam-egl.so: %s\n", dlerror());
        return -1;
    }

    get_egl_window_resource_type = dlsym(egl_nif_lib_handle, "get_egl_window_resource_type");
    if (!get_egl_window_resource_type) {
        fprintf(stderr, "failed to load symbol get_egl_window_resource_type: %s\n", dlerror());
        dlclose(egl_nif_lib_handle);
        return -1;
    }
#endif
    egl_window_resource_type = get_egl_window_resource_type(env);

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_undefined = enif_make_atom(env, "undefined");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");

    atom_press = enif_make_atom(env, "press");
    atom_release = enif_make_atom(env, "release");
    atom_repeat = enif_make_atom(env, "repeat");

    atom_no_monitor = enif_make_atom(env, "no_monitor");
    atom_no_window = enif_make_atom(env, "no_window");

    atom_glfw_error = enif_make_atom(env, "glfw_error");

    atom_glfw_monitor = enif_make_atom(env, "glfw_monitor");

    atom_glfw_video_mode = enif_make_atom(env, "glfw_video_mode");
    atom_glfw_gamma_ramp = enif_make_atom(env, "glfw_gamma_ramp");
    atom_glfw_window_position = enif_make_atom(env, "glfw_window_position");
    atom_glfw_window_size = enif_make_atom(env, "glfw_window_size");
    atom_glfw_window_close = enif_make_atom(env, "glfw_window_close");
    atom_glfw_window_refresh = enif_make_atom(env, "glfw_window_refresh");
    atom_glfw_window_focus = enif_make_atom(env, "glfw_window_focus");
    atom_glfw_window_iconify = enif_make_atom(env, "glfw_window_iconify");
    atom_glfw_window_maximize = enif_make_atom(env, "glfw_window_maximize");
    atom_glfw_window_content_scale = enif_make_atom(env, "glfw_window_content_scale");
    atom_glfw_framebuffer_size = enif_make_atom(env, "glfw_framebuffer_size");

    atom_glfw_key = enif_make_atom(env, "glfw_key");
    atom_glfw_char = enif_make_atom(env, "glfw_char");
    atom_glfw_char_mods = enif_make_atom(env, "glfw_char_mods");
    atom_glfw_mouse_button = enif_make_atom(env, "glfw_mouse_button");
    atom_glfw_cursor_position = enif_make_atom(env, "glfw_cursor_position");
    atom_glfw_cursor_enter = enif_make_atom(env, "glfw_cursor_enter");
    atom_glfw_scroll = enif_make_atom(env, "glfw_scroll");
    atom_glfw_drop = enif_make_atom(env, "glfw_drop");

    atom_glfw_joystick = enif_make_atom(env, "glfw_joystick");

    atom_press = enif_make_atom(env, "press");
    atom_release = enif_make_atom(env, "release");

    atom_not_present = enif_make_atom(env, "not_present");
    atom_joysticks[0] = enif_make_atom(env, "joystick_1");
    atom_joysticks[1] = enif_make_atom(env, "joystick_2");
    atom_joysticks[2] = enif_make_atom(env, "joystick_3");
    atom_joysticks[3] = enif_make_atom(env, "joystick_4");
    atom_joysticks[4] = enif_make_atom(env, "joystick_5");
    atom_joysticks[5] = enif_make_atom(env, "joystick_6");
    atom_joysticks[6] = enif_make_atom(env, "joystick_7");
    atom_joysticks[7] = enif_make_atom(env, "joystick_8");
    atom_joysticks[8] = enif_make_atom(env, "joystick_9");
    atom_joysticks[9] = enif_make_atom(env, "joystick_10");
    atom_joysticks[10] = enif_make_atom(env, "joystick_11");
    atom_joysticks[11] = enif_make_atom(env, "joystick_12");
    atom_joysticks[12] = enif_make_atom(env, "joystick_13");
    atom_joysticks[13] = enif_make_atom(env, "joystick_14");
    atom_joysticks[14] = enif_make_atom(env, "joystick_15");
    atom_joysticks[15] = enif_make_atom(env, "joystick_16");
    atom_connected = enif_make_atom(env, "connected");
    atom_disconnected = enif_make_atom(env, "disconnected");

    atom_axe_left_x = enif_make_atom(env, "axe_left_x");
    atom_axe_left_y = enif_make_atom(env, "axe_left_y");
    atom_axe_right_x = enif_make_atom(env, "axe_right_x");
    atom_axe_right_y = enif_make_atom(env, "axe_right_y");
    atom_axe_left_trigger = enif_make_atom(env, "axe_left_trigger");
    atom_axe_right_trigger = enif_make_atom(env, "axe_right_trigger");
    atom_button_a = enif_make_atom(env, "button_a");
    atom_button_b = enif_make_atom(env, "button_b");
    atom_button_x = enif_make_atom(env, "button_x");
    atom_button_y = enif_make_atom(env, "button_y");
    atom_button_left_bumper = enif_make_atom(env, "button_left_bumper");
    atom_button_right_bumper = enif_make_atom(env, "button_right_bumper");
    atom_button_back = enif_make_atom(env, "button_back");
    atom_button_start = enif_make_atom(env, "button_start");
    atom_button_guide = enif_make_atom(env, "button_guide");
    atom_button_left_thumb = enif_make_atom(env, "button_left_thumb");
    atom_button_right_thumb = enif_make_atom(env, "button_right_thumb");
    atom_button_dpad_up = enif_make_atom(env, "button_dpad_up");
    atom_button_dpad_right = enif_make_atom(env, "button_dpad_right");
    atom_button_dpad_down = enif_make_atom(env, "button_dpad_down");
    atom_button_dpad_left = enif_make_atom(env, "button_dpad_left");

    glfw_monitor_resource_type = enif_open_resource_type(env, NULL, "glfw_monitor", glfw_monitor_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (glfw_monitor_resource_type == NULL) {
        fprintf(stderr, "failed to open 'GLFW monitor' resource type\n");
        return -1;
    }
    glfw_window_resource_type = enif_open_resource_type(env, NULL, "glfw_window", glfw_window_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (glfw_window_resource_type == NULL) {
        fprintf(stderr, "failed to open 'GLFW window' resource type\n");
        return -1;
    }
    glfw_cursor_resource_type = enif_open_resource_type(env, NULL, "glfw_cursor", glfw_cursor_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (glfw_cursor_resource_type == NULL) {
        fprintf(stderr, "failed to open 'GLFW cursor' resource type\n");
        return -1;
    }

    glfw_error_handler_env = enif_alloc_env();
    beam_handler_clear(&glfw_error_handler);

    glfw_monitor_handler_env = enif_alloc_env();
    beam_handler_clear(&glfw_monitor_handler);

    glfw_joystick_handler_env = enif_alloc_env();
    beam_handler_clear(&glfw_joystick_handler);

    if (!command_executor_init(&command_executor)) {
        fprintf(stderr, "failed to initialize the command executor\n");
        return -1;
    }

    return 0;
}

static void nif_module_unload(ErlNifEnv* caller_env, void* priv_data)
{
    (void)caller_env;
    (void)priv_data;

    if(!command_executor_destroy(&command_executor)) {
        fprintf(stderr, "failed to destroy the command executor\n");
    }

    enif_free_env(glfw_error_handler_env);
    enif_free_env(glfw_monitor_handler_env);
    enif_free_env(glfw_joystick_handler_env);
}

static ERL_NIF_TERM glfw_init_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int hint;
    int value;

    if (!enif_get_int(env, argv[0], &hint)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &value)) {
        return enif_make_badarg(env);
    }

    glfwInitHint(hint, value);
    return atom_ok;
}

static ERL_NIF_TERM nif_init_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_init_hint, env, argc, argv);
}

static ERL_NIF_TERM glfw_init_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    int result = glfwInit();
    if (result == GLFW_TRUE) {
        beam_glfw_initialized = 1;
        glfwSetMonitorCallback(monitor_callback);
        return atom_true;
    }
    else {
        beam_glfw_initialized = 0;
        return atom_false;
    }
}

static ERL_NIF_TERM nif_init_(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_init_command, env, argc, argv);
}

static ERL_NIF_TERM glfw_terminate_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    beam_poison_all_resources(1);
    glfwTerminate();
    beam_glfw_initialized = 0;
    return atom_ok;
}

static ERL_NIF_TERM nif_terminate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_terminate_command, env, argc, argv);
}

static ERL_NIF_TERM nif_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    int major, minor, rev;
    glfwGetVersion(&major, &minor, &rev);

    return enif_make_tuple3(env,
        enif_make_int(env, major),
        enif_make_int(env, minor),
        enif_make_int(env, rev)
    );
}

static ERL_NIF_TERM nif_version_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    const char* version = glfwGetVersionString();
    return enif_make_string(env, version, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    const char* description;
    int error_code = glfwGetError(&description);

    ERL_NIF_TERM desc_term;
    if (description != NULL) {
        desc_term = enif_make_string(env, description, ERL_NIF_UTF8);
    } else {
        desc_term = atom_undefined;
    }

    return enif_make_tuple2(
        env,
        enif_make_int(env, error_code),
        desc_term
    );
}

void error_callback(int error_code, const char *description) {
    ERL_NIF_TERM code_term = atom_undefined;
    switch(error_code) {
        case GLFW_NOT_INITIALIZED:
            code_term = enif_make_atom(glfw_error_handler_env, "not_initialized");
            break;
        case GLFW_NO_CURRENT_CONTEXT:
            code_term = enif_make_atom(glfw_error_handler_env, "no_current_context");
            break;
        case GLFW_INVALID_ENUM:
            code_term = enif_make_atom(glfw_error_handler_env, "invalid_enum");
            break;
        case GLFW_INVALID_VALUE:
            code_term = enif_make_atom(glfw_error_handler_env, "invalid_value");
            break;
        case GLFW_OUT_OF_MEMORY:
            code_term = enif_make_atom(glfw_error_handler_env, "out_of_memory");
            break;
        // case GLFW_API_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "api_unavailable");
        //     break;
        // case GLFW_VERSION_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "version_unavailable");
        //     break;
        // case GLFW_PLATFORM_ERROR:
        //     code_term = enif_make_atom(glfw_error_handler_env, "platform_error");
        //     break;
        // case GLFW_FORMAT_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "format_unavailable");
        //     break;
        // case GLFW_NO_WINDOW_CONTEXT:
        //     code_term = enif_make_atom(glfw_error_handler_env, "no_window_context");
        //     break;
        // case GLFW_CURSOR_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "cursor_unavailable");
        //     break;
        // case GLFW_FEATURE_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "feature_unavailable");
        //     break;
        // case GLFW_FEATURE_UNIMPLEMENTED:
        //     code_term = enif_make_atom(glfw_error_handler_env, "feature_unimplemented");
        //     break;
        // case GLFW_PLATFORM_UNAVAILABLE:
        //     code_term = enif_make_atom(glfw_error_handler_env, "platform_unavailable");
        //     break;
        default:
            code_term = atom_undefined;
    }

    ERL_NIF_TERM description_term;
    if (description != NULL) {
        description_term = enif_make_string(glfw_error_handler_env, description, ERL_NIF_UTF8);
    } else {
        description_term = atom_undefined;
    }

    ERL_NIF_TERM result = enif_make_tuple3(
        glfw_error_handler_env,
        atom_glfw_error,
        code_term,
        description_term
    );

    beam_handler_send(glfw_error_handler_env, &glfw_error_handler, result);
}

static ERL_NIF_TERM nif_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    return beam_handler_to_term(env, &glfw_error_handler);
}

static ERL_NIF_TERM glfw_set_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (!beam_handler_from_term(env, argv[0], &glfw_error_handler)) {
        return enif_make_badarg(env);
    }
    if (glfw_error_handler.set) {
        glfwSetErrorCallback(error_callback);
    } else {
        glfwSetErrorCallback(NULL);
    }
    return atom_ok;
}

static ERL_NIF_TERM nif_set_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_error_handler, env, argc, argv);
}

static ERL_NIF_TERM nif_platform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
#if defined(BEAM_GLFW_HAS_GET_PLATFORM)
    int platform = glfwGetPlatform();
    return enif_make_int(env, platform);
#else
    if (!beam_glfw_initialized) {
        return enif_make_int(env, 0);
    }
#if defined(_WIN32)
    return enif_make_int(env, GLFW_PLATFORM_WIN32);
#elif defined(__APPLE__)
    return enif_make_int(env, GLFW_PLATFORM_COCOA);
#elif defined(BEAM_GLFW_HAS_NATIVE_WAYLAND) && !defined(BEAM_GLFW_HAS_NATIVE_X11)
    return enif_make_int(env, GLFW_PLATFORM_WAYLAND);
#else
    return enif_make_int(env, GLFW_PLATFORM_X11);
#endif
#endif
}

static ERL_NIF_TERM nif_platform_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    int platform;
    if (!enif_get_int(env, argv[0], &platform)) {
        return enif_make_badarg(env);
    }

#if defined(BEAM_GLFW_HAS_GET_PLATFORM)
    int supported = glfwPlatformSupported(platform);
#else
    int supported = GLFW_FALSE;
#if defined(_WIN32)
    if (platform == GLFW_PLATFORM_WIN32) {
        supported = GLFW_TRUE;
    }
#elif defined(__APPLE__)
    if (platform == GLFW_PLATFORM_COCOA) {
        supported = GLFW_TRUE;
    }
#else
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    if (platform == GLFW_PLATFORM_WAYLAND) {
        supported = GLFW_TRUE;
    }
#endif
#if defined(BEAM_GLFW_HAS_NATIVE_X11)
    if (platform == GLFW_PLATFORM_X11) {
        supported = GLFW_TRUE;
    }
#endif
#endif
#endif
    if (supported == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM glfw_monitors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    int count;
    GLFWmonitor** monitors = glfwGetMonitors(&count);
    if (!monitors) {
        return enif_make_list(env, 0);
    }

    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (int i = count - 1; i >= 0; i--) {
        list = enif_make_list_cell(env, beam_intern_monitor(env, monitors[i]), list);
    }

    return list;
}

static ERL_NIF_TERM nif_monitors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitors, env, argc, argv);
}

static ERL_NIF_TERM glfw_primary_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    GLFWmonitor* monitor = glfwGetPrimaryMonitor();
    if (!monitor) {
        return atom_undefined;
    }

    return beam_intern_monitor(env, monitor);
}

static ERL_NIF_TERM nif_primary_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_primary_monitor, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    int x, y;
    glfwGetMonitorPos(*monitor, &x, &y);

    return enif_make_tuple2(
        env,
        enif_make_int(env, x),
        enif_make_int(env, y)
    );
}

static ERL_NIF_TERM nif_monitor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_position, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_work_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    int x, y, width, height;
    glfwGetMonitorWorkarea(*monitor, &x, &y, &width, &height);

    return enif_make_tuple4(
        env,
        enif_make_int(env, x),
        enif_make_int(env, y),
        enif_make_int(env, width),
        enif_make_int(env, height)
    );
}

static ERL_NIF_TERM nif_monitor_work_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_work_area, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_physical_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    int width, height;
    glfwGetMonitorPhysicalSize(*monitor, &width, &height);

    return enif_make_tuple2(
        env,
        enif_make_int(env, width),
        enif_make_int(env, height)
    );
}

static ERL_NIF_TERM nif_monitor_physical_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_physical_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_content_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    float x_scale, y_scale;
    glfwGetMonitorContentScale(*monitor, &x_scale, &y_scale);

    return enif_make_tuple2(
        env,
        enif_make_double(env, x_scale),
        enif_make_double(env, y_scale)
    );
}

static ERL_NIF_TERM nif_monitor_content_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_content_scale, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    const char* name = glfwGetMonitorName(*monitor);
    if (name == NULL) {
        return atom_undefined;
    }
    return enif_make_string(env, name, ERL_NIF_UTF8);
}

static ERL_NIF_TERM nif_monitor_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_name, env, argc, argv);
}

static void monitor_callback(GLFWmonitor *monitor, int event) {
    enif_clear_env(glfw_monitor_handler_env);
    ERL_NIF_TERM monitor_term = beam_intern_monitor(glfw_monitor_handler_env, monitor);
    ERL_NIF_TERM result = enif_make_tuple3(
        glfw_monitor_handler_env,
        atom_glfw_monitor,
        monitor_term,
        event == GLFW_CONNECTED ? atom_connected : atom_disconnected
    );
    beam_handler_send(glfw_monitor_handler_env, &glfw_monitor_handler, result);
    if (event == GLFW_DISCONNECTED) {
        GLFWMonitorResource* it;
        for (it = interned_monitors; it != NULL; it = it->intern_next) {
            if (it->alive && it->monitor == monitor) {
                beam_poison_monitor(it);
                break;
            }
        }
    }
}

static ERL_NIF_TERM nif_monitor_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    return beam_handler_to_term(env, &glfw_monitor_handler);
}

static ERL_NIF_TERM glfw_monitor_set_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (!beam_handler_from_term(env, argv[0], &glfw_monitor_handler)) {
        return enif_make_badarg(env);
    }
    return atom_ok;
}

static ERL_NIF_TERM nif_monitor_set_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_set_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_video_modes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    int count;
    const GLFWvidmode* modes = glfwGetVideoModes(*monitor, &count);
    if (!modes) {
        return atom_undefined;
    }

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = count - 1; i >= 0; i--) {
        ERL_NIF_TERM mode = enif_make_tuple7(
            env,
            atom_glfw_video_mode,
            enif_make_int(env, modes[i].width),
            enif_make_int(env, modes[i].height),
            enif_make_int(env, modes[i].redBits),
            enif_make_int(env, modes[i].greenBits),
            enif_make_int(env, modes[i].blueBits),
            enif_make_int(env, modes[i].refreshRate)
        );

        list = enif_make_list_cell(env, mode, list);
    }

    return list;
}

static ERL_NIF_TERM nif_video_modes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_video_modes, env, argc, argv);
}

static ERL_NIF_TERM glfw_video_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    const GLFWvidmode* mode = glfwGetVideoMode(*monitor);
    if (!mode) {
        return atom_undefined;
    }

    return enif_make_tuple7(env,
        atom_glfw_video_mode,
        enif_make_int(env, mode->width),
        enif_make_int(env, mode->height),
        enif_make_int(env, mode->redBits),
        enif_make_int(env, mode->greenBits),
        enif_make_int(env, mode->blueBits),
        enif_make_int(env, mode->refreshRate)
    );
}

static ERL_NIF_TERM nif_video_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_video_mode, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_gamma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    double gamma;
    if (!enif_get_double(env, argv[1], &gamma)) {
        return enif_make_badarg(env);
    }

    glfwSetGamma(*monitor, gamma);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_gamma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_gamma, env, argc, argv);
}

static ERL_NIF_TERM glfw_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    const GLFWgammaramp* ramp = glfwGetGammaRamp(*monitor);
    if (ramp == NULL) {
        return atom_undefined;
    }

    ERL_NIF_TERM red_list = enif_make_list(env, 0);
    ERL_NIF_TERM green_list = enif_make_list(env, 0);
    ERL_NIF_TERM blue_list = enif_make_list(env, 0);

    for (unsigned int i = ramp->size; i > 0; i--) {
        red_list = enif_make_list_cell(env,
            enif_make_uint(env, ramp->red[i-1]), red_list);
        green_list = enif_make_list_cell(env,
            enif_make_uint(env, ramp->green[i-1]), green_list);
        blue_list = enif_make_list_cell(env,
            enif_make_uint(env, ramp->blue[i-1]), blue_list);
    }

    return enif_make_tuple4(env,
        atom_glfw_gamma_ramp,
        red_list,
        green_list,
        blue_list
    );
}

static ERL_NIF_TERM nif_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_gamma_ramp, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // XXX: Review implementation.

    GLFWMonitorResource* monitor_resource;
    if (!beam_get_monitor(env, argv[0], &monitor_resource)) {
        return enif_make_badarg(env);
    }
    GLFWmonitor** monitor = &monitor_resource->monitor;

    const ERL_NIF_TERM* terms;
    int arity;
    if (!enif_get_tuple(env, argv[1], &arity, &terms) || arity != 4) {
        return enif_make_badarg(env);
    }

    unsigned int length;
    if (!enif_get_list_length(env, terms[1], &length)) {
        return enif_make_badarg(env);
    }

    unsigned short* red = malloc(length * sizeof(unsigned short));
    unsigned short* green = malloc(length * sizeof(unsigned short));
    unsigned short* blue = malloc(length * sizeof(unsigned short));

    ERL_NIF_TERM list, head, tail;
    unsigned int val;

    list = terms[1];
    for (unsigned int i = 0; i < length && enif_get_list_cell(env, list, &head, &tail); i++) {
        if (!enif_get_uint(env, head, &val)) {
            goto error;
        }
        red[i] = (unsigned short)val;
        list = tail;
    }

    list = terms[2];
    for (unsigned int i = 0; i < length && enif_get_list_cell(env, list, &head, &tail); i++) {
        if (!enif_get_uint(env, head, &val)) {
            goto error;
        }
        green[i] = (unsigned short)val;
        list = tail;
    }

    list = terms[3];
    for (unsigned int i = 0; i < length && enif_get_list_cell(env, list, &head, &tail); i++) {
        if (!enif_get_uint(env, head, &val)) {
            goto error;
        }
        blue[i] = (unsigned short)val;
        list = tail;
    }

    GLFWgammaramp ramp = {
        .red = red,
        .green = green,
        .blue = blue,
        .size = length
    };

    glfwSetGammaRamp(*monitor, &ramp);

    free(red);
    free(green);
    free(blue);
    return atom_ok;

error:
    free(red);
    free(green);
    free(blue);
    return enif_make_badarg(env);
}

static ERL_NIF_TERM nif_set_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_gamma_ramp, env, argc, argv);
}

static ERL_NIF_TERM glfw_default_window_hints(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    glfwDefaultWindowHints();
    return atom_ok;
}

static ERL_NIF_TERM nif_default_window_hints(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_default_window_hints, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int hint, value;
    if (!enif_get_int(env, argv[0], &hint)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &value)) {
        return enif_make_badarg(env);
    }

    glfwWindowHint(hint, value);
    return atom_ok;
}

static ERL_NIF_TERM nif_window_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_hint, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_hint_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int hint;
    if (!enif_get_int(env, argv[0], &hint)) {
        return enif_make_badarg(env);
    }

    char* value = beam_alloc_utf8(env, argv[1]);
    if (value == NULL) {
        return enif_make_badarg(env);
    }

    glfwWindowHintString(hint, value);
    free(value);
    return atom_ok;
}

static ERL_NIF_TERM nif_window_hint_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_hint_string, env, argc, argv);
}

static ERL_NIF_TERM glfw_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int width, height;
    if (!enif_get_int(env, argv[0], &width)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &height)) {
        return enif_make_badarg(env);
    }
    char* title = beam_alloc_utf8(env, argv[2]);
    if (title == NULL) {
        return enif_make_badarg(env);
    }

    // XXX: This will be removed after window hints are implemented.
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    GLFWwindow* window = glfwCreateWindow(width, height, title, NULL, NULL);
    free(title);
    if (!window) {
        return atom_no_window;
    }

    GLFWWindowResource* window_resource = enif_alloc_resource(
        glfw_window_resource_type,
        sizeof(GLFWWindowResource)
    );
    window_resource->env = enif_alloc_env();
    window_resource->window = window;
    window_resource->alive = 1;
    beam_handler_clear(&window_resource->window_position_handler);
    beam_handler_clear(&window_resource->window_size_handler);
    beam_handler_clear(&window_resource->window_close_handler);
    beam_handler_clear(&window_resource->window_refresh_handler);
    beam_handler_clear(&window_resource->window_focus_handler);
    beam_handler_clear(&window_resource->window_iconify_handler);
    beam_handler_clear(&window_resource->window_maximize_handler);
    beam_handler_clear(&window_resource->window_content_scale_handler);
    beam_handler_clear(&window_resource->framebuffer_size_handler);
    beam_handler_clear(&window_resource->key_handler);
    beam_handler_clear(&window_resource->char_handler);
    beam_handler_clear(&window_resource->char_mods_handler);
    beam_handler_clear(&window_resource->mouse_button_handler);
    beam_handler_clear(&window_resource->cursor_position_handler);
    beam_handler_clear(&window_resource->cursor_enter_handler);
    beam_handler_clear(&window_resource->scroll_handler);
    beam_handler_clear(&window_resource->drop_handler);
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    window_resource->wayland_egl_window = NULL;
#endif
    window_resource->live_next = live_windows;
    live_windows = window_resource;
    enif_keep_resource(window_resource);

    glfwSetWindowUserPointer(window, window_resource);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    ERL_NIF_TERM window_term = enif_make_resource(env, window_resource);
    enif_release_resource(window_resource);

    window_resource->window_term = enif_make_copy(window_resource->env, window_term);

    return enif_make_tuple2(
        env,
        atom_ok,
        window_term
    );
}

static ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_create_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_destroy_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    beam_poison_window(window_resource, 1);
    return atom_ok;
}

static ERL_NIF_TERM nif_destroy_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_destroy_window, env, argc, argv);
}

static ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int result = glfwWindowShouldClose(window);
    if (result == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM nif_set_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int value;
    if (enif_is_identical(argv[1], atom_true)) {
        value = GLFW_TRUE;
    } else if (enif_is_identical(argv[1], atom_false)) {
        value = GLFW_FALSE;
    } else {
        return enif_make_badarg(env);
    }

    glfwSetWindowShouldClose(window, value);
    return atom_ok;
}

static ERL_NIF_TERM glfw_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    const char* title = glfwGetWindowTitle(window);
    if (title == NULL) {
        return atom_undefined;
    }
    return enif_make_string(env, title, ERL_NIF_UTF8);
}

static ERL_NIF_TERM nif_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_title, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    char* title = beam_alloc_utf8(env, argv[1]);
    if (title == NULL) {
        return enif_make_badarg(env);
    }

    glfwSetWindowTitle(window, title);
    free(title);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_title, env, argc, argv);
}

static ERL_NIF_TERM nif_set_window_icon(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    return enif_make_int(env, 42);
}

static ERL_NIF_TERM glfw_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int x, y;
    glfwGetWindowPos(window, &x, &y);

    return enif_make_tuple2(
        env,
        enif_make_int(env, x),
        enif_make_int(env, y)
    );
}

static ERL_NIF_TERM nif_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_position, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    const ERL_NIF_TERM* xy;
    int arity;
    if (!enif_get_tuple(env, argv[1], &arity, &xy) || arity != 2) {
        return enif_make_badarg(env);
    }

    int x, y;
    if (!enif_get_int(env, xy[0], &x) || !enif_get_int(env, xy[1], &y)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowPos(window, x, y);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_position, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int width, height;
    glfwGetWindowSize(window, &width, &height);

    return enif_make_tuple2(
        env,
        enif_make_int(env, width),
        enif_make_int(env, height)
    );
}

static ERL_NIF_TERM nif_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_framebuffer_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int width, height;
    glfwGetFramebufferSize(window, &width, &height);

    return enif_make_tuple2(
        env,
        enif_make_int(env, width),
        enif_make_int(env, height)
    );
}

static ERL_NIF_TERM nif_framebuffer_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_framebuffer_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    const ERL_NIF_TERM* wh;
    int arity;
    if (!enif_get_tuple(env, argv[1], &arity, &wh) || arity != 2) {
        return enif_make_badarg(env);
    }

    int width, height;
    if (!enif_get_int(env, wh[0], &width) || !enif_get_int(env, wh[1], &height)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowSize(window, width, height);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_size_limits(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int minwidth, minheight, maxwidth, maxheight;
    if (!enif_get_int(env, argv[1], &minwidth) ||
        !enif_get_int(env, argv[2], &minheight) ||
        !enif_get_int(env, argv[3], &maxwidth) ||
        !enif_get_int(env, argv[4], &maxheight)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowSizeLimits(window, minwidth, minheight, maxwidth, maxheight);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_size_limits(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_size_limits, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_aspect_ratio(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int numer, denom;
    if (!enif_get_int(env, argv[1], &numer) || !enif_get_int(env, argv[2], &denom)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowAspectRatio(window, numer, denom);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_aspect_ratio(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_aspect_ratio, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_frame_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int left, top, right, bottom;
    glfwGetWindowFrameSize(window, &left, &top, &right, &bottom);

    return enif_make_tuple4(
        env,
        enif_make_int(env, left),
        enif_make_int(env, top),
        enif_make_int(env, right),
        enif_make_int(env, bottom)
    );
}

static ERL_NIF_TERM nif_window_frame_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_frame_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_content_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    float x_scale, y_scale;
    glfwGetWindowContentScale(window, &x_scale, &y_scale);

    return enif_make_tuple2(
        env,
        enif_make_double(env, x_scale),
        enif_make_double(env, y_scale)
    );
}

static ERL_NIF_TERM nif_window_content_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_content_scale, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    float opacity = glfwGetWindowOpacity(window);
    return enif_make_double(env, opacity);
}

static ERL_NIF_TERM nif_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_opacity, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    double opacity;
    if (!enif_get_double(env, argv[1], &opacity)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowOpacity(window, opacity);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_opacity, env, argc, argv);
}

static ERL_NIF_TERM glfw_iconify_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwIconifyWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_iconify_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_iconify_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_restore_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwRestoreWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_restore_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_restore_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_maximize_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwMaximizeWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_maximize_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_maximize_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_show_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwShowWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_show_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_show_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_hide_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwHideWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_hide_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_hide_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_focus_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwFocusWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_focus_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_focus_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_request_window_attention(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwRequestWindowAttention(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_request_window_attention(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_request_window_attention, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    GLFWmonitor* monitor = glfwGetWindowMonitor(window);
    if (monitor == NULL) {
        return atom_undefined;
    }
    return beam_intern_monitor(env, monitor);
}

static ERL_NIF_TERM nif_window_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_monitor, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    GLFWmonitor* monitor_ptr = NULL;
    if (!enif_is_identical(argv[1], atom_undefined)) {
        GLFWMonitorResource* monitor_resource;
        if (!beam_get_monitor(env, argv[1], &monitor_resource)) {
            return enif_make_badarg(env);
        }
        monitor_ptr = monitor_resource->monitor;
    }

    int xpos, ypos, width, height, refresh_rate;
    if (!enif_get_int(env, argv[2], &xpos) ||
        !enif_get_int(env, argv[3], &ypos) ||
        !enif_get_int(env, argv[4], &width) ||
        !enif_get_int(env, argv[5], &height) ||
        !enif_get_int(env, argv[6], &refresh_rate)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowMonitor(window, monitor_ptr, xpos, ypos, width, height, refresh_rate);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_monitor, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int attrib;
    if (!enif_get_int(env, argv[1], &attrib)) {
        return enif_make_badarg(env);
    }
    int value = glfwGetWindowAttrib(window, attrib);

    // All values happen to be either true or false, but that might change in
    // later version of GLFW.
    if (value == GLFW_TRUE) {
        return atom_true;
    } else if (value == GLFW_FALSE) {
        return atom_false;
    } else {
        return atom_undefined;
    }
}

static ERL_NIF_TERM nif_window_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_attrib, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int attrib;
    if (!enif_get_int(env, argv[1], &attrib)) {
        return enif_make_badarg(env);
    }
    int value;
    if (!enif_get_int(env, argv[2], &value)) {
        return enif_make_badarg(env);
    }
    glfwSetWindowAttrib(window, attrib, value);

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_attrib, env, argc, argv);
}

void window_position_callback(GLFWwindow* window, int xpos, int ypos) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        enif_make_int(window_resource->env, xpos),
        enif_make_int(window_resource->env, ypos)
    );

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_position,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->window_position_handler, result);
}

static ERL_NIF_TERM nif_window_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_position_handler);
}

static ERL_NIF_TERM glfw_set_window_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_position_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_position_handler.set) {
        glfwSetWindowPosCallback(window, window_position_callback);
    } else {
        glfwSetWindowPosCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_position_handler, env, argc, argv);
}

void window_size_callback(GLFWwindow* window, int width, int height) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        enif_make_int(window_resource->env, width),
        enif_make_int(window_resource->env, height)
    );

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_size,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->window_size_handler, result);
}

static ERL_NIF_TERM nif_window_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_size_handler);
}

static ERL_NIF_TERM glfw_set_window_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_size_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_size_handler.set) {
        glfwSetWindowSizeCallback(window, window_size_callback);
    } else {
        glfwSetWindowSizeCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_size_handler, env, argc, argv);
}

void window_close_callback(GLFWwindow* window) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple2(
        window_resource->env,
        atom_glfw_window_close,
        window_resource->window_term
    );

    beam_handler_send(window_resource->env, &window_resource->window_close_handler, result);
}

static ERL_NIF_TERM nif_window_close_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_close_handler);
}

static ERL_NIF_TERM glfw_set_window_close_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_close_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_close_handler.set) {
        glfwSetWindowCloseCallback(window, window_close_callback);
    } else {
        glfwSetWindowCloseCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_close_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_close_handler, env, argc, argv);
}

void window_refresh_callback(GLFWwindow* window) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple2(
        window_resource->env,
        atom_glfw_window_refresh,
        window_resource->window_term
    );

    beam_handler_send(window_resource->env, &window_resource->window_refresh_handler, result);
}

static ERL_NIF_TERM nif_window_refresh_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_refresh_handler);
}

static ERL_NIF_TERM glfw_set_window_refresh_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_refresh_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_refresh_handler.set) {
        glfwSetWindowRefreshCallback(window, window_refresh_callback);
    } else {
        glfwSetWindowRefreshCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_refresh_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_refresh_handler, env, argc, argv);
}

void window_focus_callback(GLFWwindow* window, int focused) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_focus,
        window_resource->window_term,
        focused ? atom_true : atom_false
    );

    beam_handler_send(window_resource->env, &window_resource->window_focus_handler, result);
}

static ERL_NIF_TERM nif_window_focus_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_focus_handler);
}

static ERL_NIF_TERM glfw_set_window_focus_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_focus_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_focus_handler.set) {
        glfwSetWindowFocusCallback(window, window_focus_callback);
    } else {
        glfwSetWindowFocusCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_focus_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_focus_handler, env, argc, argv);
}

void window_iconify_callback(GLFWwindow* window, int iconified) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_iconify,
        window_resource->window_term,
        iconified ? atom_true : atom_false
    );

    beam_handler_send(window_resource->env, &window_resource->window_iconify_handler, result);
}

static ERL_NIF_TERM nif_window_iconify_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_iconify_handler);
}

static ERL_NIF_TERM glfw_set_window_iconify_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_iconify_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_iconify_handler.set) {
        glfwSetWindowIconifyCallback(window, window_iconify_callback);
    } else {
        glfwSetWindowIconifyCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_iconify_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_iconify_handler, env, argc, argv);
}

void window_maximize_callback(GLFWwindow* window, int maximized) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_maximize,
        window_resource->window_term,
        maximized ? atom_true : atom_false
    );

    beam_handler_send(window_resource->env, &window_resource->window_maximize_handler, result);
}

static ERL_NIF_TERM nif_window_maximize_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_maximize_handler);
}

static ERL_NIF_TERM glfw_set_window_maximize_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_maximize_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_maximize_handler.set) {
        glfwSetWindowMaximizeCallback(window, window_maximize_callback);
    } else {
        glfwSetWindowMaximizeCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_maximize_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_maximize_handler, env, argc, argv);
}

void window_content_scale_callback(GLFWwindow* window, float xscale, float yscale) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        enif_make_int(window_resource->env, xscale),
        enif_make_int(window_resource->env, yscale)
    );

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_window_content_scale,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->window_content_scale_handler, result);
}

static ERL_NIF_TERM nif_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->window_content_scale_handler);
}

static ERL_NIF_TERM glfw_set_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->window_content_scale_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->window_content_scale_handler.set) {
        glfwSetWindowContentScaleCallback(window, window_content_scale_callback);
    } else {
        glfwSetWindowContentScaleCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_content_scale_handler, env, argc, argv);
}

static void framebuffer_size_callback(GLFWwindow* window, int width, int height) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    if (window_resource->wayland_egl_window != NULL) {
        wl_egl_window_resize(window_resource->wayland_egl_window, width, height, 0, 0);
    }
#endif

    if (!window_resource->alive || !window_resource->framebuffer_size_handler.set) {
        return;
    }

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        enif_make_int(window_resource->env, width),
        enif_make_int(window_resource->env, height)
    );

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_framebuffer_size,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->framebuffer_size_handler, result);
}

static ERL_NIF_TERM nif_framebuffer_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->framebuffer_size_handler);
}

static ERL_NIF_TERM glfw_set_framebuffer_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    // The GLFW framebuffer-size callback stays installed for Wayland
    // wl_egl_window resize. Clearing the handler only stops BEAM messages.
    if (!beam_handler_from_term(env, argv[1], &window_resource->framebuffer_size_handler)) {
        return enif_make_badarg(env);
    }
    return atom_ok;
}

static ERL_NIF_TERM nif_set_framebuffer_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_framebuffer_size_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    glfwPollEvents();
    return atom_ok;
}

static ERL_NIF_TERM nif_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_poll_events, env, argc, argv);
}

static ERL_NIF_TERM glfw_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    glfwPostEmptyEvent();
    return atom_ok;
}

static ERL_NIF_TERM nif_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_post_empty_event, env, argc, argv);
}

static ERL_NIF_TERM glfw_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int mode;
    if (!enif_get_int(env, argv[1], &mode)) {
        return enif_make_badarg(env);
    }

    int value = glfwGetInputMode(window, mode);
    return enif_make_int(env, value);
}

static ERL_NIF_TERM nif_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_input_mode, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int mode;
    if (!enif_get_int(env, argv[1], &mode)) {
        return enif_make_badarg(env);
    }

    int value;
    if (!enif_get_int(env, argv[2], &value)) {
        return enif_make_badarg(env);
    }

    glfwSetInputMode(window, mode, value);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_input_mode, env, argc, argv);
}

static ERL_NIF_TERM glfw_create_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int width, height, xhot, yhot;
    ErlNifBinary pix_bin;
    GLFWimage image;

    if (!enif_get_int(env, argv[0], &width) ||
        !enif_get_int(env, argv[1], &height) ||
        !enif_inspect_binary(env, argv[2], &pix_bin) ||
        !enif_get_int(env, argv[3], &xhot) ||
        !enif_get_int(env, argv[4], &yhot)) {
        return enif_make_badarg(env);
    }

    image.width = width;
    image.height = height;
    image.pixels = (unsigned char*) pix_bin.data;

    GLFWcursor* cursor = glfwCreateCursor(&image, xhot, yhot);
    if (!cursor) {
        return atom_error;
    }

    GLFWCursorResource* cursor_resource = enif_alloc_resource(
        glfw_cursor_resource_type,
        sizeof(GLFWCursorResource)
    );
    cursor_resource->cursor = cursor;
    cursor_resource->alive = 1;
    cursor_resource->live_next = live_cursors;
    live_cursors = cursor_resource;
    enif_keep_resource(cursor_resource);

    ERL_NIF_TERM cursor_ref = enif_make_resource(env, cursor_resource);
    enif_release_resource(cursor_resource);

    return enif_make_tuple2(
        env,
        atom_ok,
        cursor_ref
    );
}

static ERL_NIF_TERM nif_create_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_create_cursor, env, argc, argv);
}

static ERL_NIF_TERM glfw_create_standard_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int shape;
    if (!enif_get_int(env, argv[0], &shape)) {
        return enif_make_badarg(env);
    }

    GLFWcursor* cursor = glfwCreateStandardCursor(shape);
    if (!cursor) {
        return atom_error;
    }

    GLFWCursorResource* cursor_resource = enif_alloc_resource(
        glfw_cursor_resource_type,
        sizeof(GLFWCursorResource)
    );
    cursor_resource->cursor = cursor;
    cursor_resource->alive = 1;
    cursor_resource->live_next = live_cursors;
    live_cursors = cursor_resource;
    enif_keep_resource(cursor_resource);

    ERL_NIF_TERM cursor_ref = enif_make_resource(env, cursor_resource);
    enif_release_resource(cursor_resource);

    return enif_make_tuple2(
        env,
        atom_ok,
        cursor_ref
    );
}

static ERL_NIF_TERM nif_create_standard_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_create_standard_cursor, env, argc, argv);
}

static ERL_NIF_TERM glfw_destroy_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWCursorResource* cursor_resource;
    if (!beam_get_cursor(env, argv[0], &cursor_resource)) {
        return enif_make_badarg(env);
    }
    beam_poison_cursor(cursor_resource, 1);
    return atom_ok;
}

static ERL_NIF_TERM nif_destroy_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_destroy_cursor, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    GLFWcursor* cursor_value = NULL;
    char atom_buf[32];

    if (enif_is_atom(env, argv[1])) {
        if (enif_get_atom(env, argv[1], atom_buf, sizeof(atom_buf), ERL_NIF_LATIN1) &&
            strcmp(atom_buf, "default") == 0) {
            cursor_value = NULL;
        } else {
            return enif_make_badarg(env);
        }
    } else {
        GLFWCursorResource* cursor_resource;
        if (!beam_get_cursor(env, argv[1], &cursor_resource)) {
            return enif_make_badarg(env);
        }
        cursor_value = cursor_resource->cursor;
    }

    glfwSetCursor(window, cursor_value);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_cursor, env, argc, argv);
}

static ERL_NIF_TERM glfw_raw_mouse_motion_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    int result = glfwRawMouseMotionSupported();
    if (result == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM nif_raw_mouse_motion_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_raw_mouse_motion_supported, env, argc, argv);
}

static ERL_NIF_TERM glfw_key_name_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // The input is a key code that is not GLFW_KEY_UNKNOWN.
    int key;
    if (!enif_get_int(env, argv[0], &key)) {
        return enif_make_badarg(env);
    }

    const char* name = glfwGetKeyName(key, 0);
    if (name == NULL) {
        return atom_undefined;
    }

    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_key_name_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_key_name_key, env, argc, argv);
}

static ERL_NIF_TERM glfw_key_name_scancode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int scancode;
    if (!enif_get_int(env, argv[0], &scancode)) {
        return enif_make_badarg(env);
    }

    const char* name = glfwGetKeyName(GLFW_KEY_UNKNOWN, scancode);
    if (name == NULL) {
        return atom_undefined;
    }

    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_key_name_scancode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_key_name_scancode, env, argc, argv);
}

static ERL_NIF_TERM glfw_key_scancode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int key;
    if (!enif_get_int(env, argv[0], &key)) {
        return enif_make_badarg(env);
    }

    int scancode = glfwGetKeyScancode(key);
    return enif_make_int(env, scancode);
}

static ERL_NIF_TERM nif_key_scancode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_key_scancode, env, argc, argv);
}

static ERL_NIF_TERM glfw_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int key;
    if (!enif_get_int(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }

    int action;
    action = glfwGetKey(window, key);
    if (action == GLFW_PRESS) {
        return atom_press;
    }
    else if (action == GLFW_RELEASE) {
        return atom_release;
    }

    return -1;
}

static ERL_NIF_TERM nif_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_key, env, argc, argv);
}

static ERL_NIF_TERM glfw_mouse_button(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int button;
    if (!enif_get_int(env, argv[1], &button)) {
        return enif_make_badarg(env);
    }

    int action;
    action = glfwGetMouseButton(window, button);
    if (action == GLFW_PRESS) {
        return atom_press;
    }
    else if (action == GLFW_RELEASE) {
        return atom_release;
    }

    return -1;
}

static ERL_NIF_TERM nif_mouse_button(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_mouse_button, env, argc, argv);
}

static ERL_NIF_TERM glfw_cursor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    double xpos, ypos;
    glfwGetCursorPos(window, &xpos, &ypos);

    return enif_make_tuple2(
        env,
        enif_make_double(env, xpos),
        enif_make_double(env, ypos)
    );
}

static ERL_NIF_TERM nif_cursor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_cursor_position, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_cursor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    double xpos, ypos;

    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[1], &xpos) || !enif_get_double(env, argv[2], &ypos)) {
        return enif_make_badarg(env);
    }

    glfwSetCursorPos(window_resource->window, xpos, ypos);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_cursor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_cursor_position, env, argc, argv);
}

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods) {

    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM key_term = enif_make_int(window_resource->env, key);
    ERL_NIF_TERM scancode_term = enif_make_int(window_resource->env, scancode);
    ERL_NIF_TERM action_term;
    if (action == GLFW_PRESS) {
        action_term = atom_press;
    } else if (action == GLFW_RELEASE) {
        action_term = atom_release;
    } else if (action == GLFW_REPEAT) {
        action_term = atom_repeat;
    } else {
        action_term = atom_undefined;
    }
    ERL_NIF_TERM mods_term = enif_make_int(window_resource->env, mods);

    ERL_NIF_TERM result = enif_make_tuple6(
        window_resource->env,
        atom_glfw_key,
        window_resource->window_term,
        key_term,
        scancode_term,
        action_term,
        mods_term
    );

    beam_handler_send(window_resource->env, &window_resource->key_handler, result);
}

static ERL_NIF_TERM nif_key_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->key_handler);
}

static ERL_NIF_TERM glfw_set_key_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->key_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->key_handler.set) {
        glfwSetKeyCallback(window, key_callback);
    } else {
        glfwSetKeyCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_key_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_key_handler, env, argc, argv);
}

void char_callback(GLFWwindow *window, unsigned int codepoint) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM codepoint_term = enif_make_uint(window_resource->env, codepoint);

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_char,
        window_resource->window_term,
        codepoint_term
    );

    beam_handler_send(window_resource->env, &window_resource->char_handler, result);
}

static ERL_NIF_TERM nif_character_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->char_handler);
}
static ERL_NIF_TERM glfw_set_character_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->char_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->char_handler.set) {
        glfwSetCharCallback(window, char_callback);
    } else {
        glfwSetCharCallback(window, NULL);
    }

    return atom_ok;
}
static ERL_NIF_TERM nif_set_character_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_character_handler, env, argc, argv);
}

void char_mods_callback(GLFWwindow *window, unsigned int codepoint, int mods) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM codepoint_term = enif_make_uint(window_resource->env, codepoint);
    ERL_NIF_TERM mods_term = enif_make_int(window_resource->env, mods);

    ERL_NIF_TERM result = enif_make_tuple4(
        window_resource->env,
        atom_glfw_char_mods,
        window_resource->window_term,
        codepoint_term,
        mods_term
    );

    beam_handler_send(window_resource->env, &window_resource->char_mods_handler, result);
}

static ERL_NIF_TERM nif_character_mods_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->char_mods_handler);
}
static ERL_NIF_TERM glfw_set_character_mods_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->char_mods_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->char_mods_handler.set) {
        glfwSetCharModsCallback(window, char_mods_callback);
    } else {
        glfwSetCharModsCallback(window, NULL);
    }

    return atom_ok;
}
static ERL_NIF_TERM nif_set_character_mods_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_character_mods_handler, env, argc, argv);
}

void mouse_button_callback(GLFWwindow *window, int button, int action, int mods) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM button_term = enif_make_int(window_resource->env, button);
    ERL_NIF_TERM action_term;
    if (action == GLFW_PRESS) {
        action_term = atom_press;
    } else if (action == GLFW_RELEASE) {
        action_term = atom_release;
    } else {
        action_term = atom_undefined;
    }
    ERL_NIF_TERM mods_term = enif_make_int(window_resource->env, mods);

    ERL_NIF_TERM result = enif_make_tuple5(
        window_resource->env,
        atom_glfw_mouse_button,
        window_resource->window_term,
        button_term,
        action_term,
        mods_term
    );

    beam_handler_send(window_resource->env, &window_resource->mouse_button_handler, result);
}

static ERL_NIF_TERM nif_mouse_button_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->mouse_button_handler);
}
static ERL_NIF_TERM glfw_set_mouse_button_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->mouse_button_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->mouse_button_handler.set) {
        glfwSetMouseButtonCallback(window, mouse_button_callback);
    } else {
        glfwSetMouseButtonCallback(window, NULL);
    }

    return atom_ok;
}
static ERL_NIF_TERM nif_set_mouse_button_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_mouse_button_handler, env, argc, argv);
}

void cursor_position_callback(GLFWwindow *window, double xpos, double ypos) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM xpos_term = enif_make_double(window_resource->env, xpos);
    ERL_NIF_TERM ypos_term = enif_make_double(window_resource->env, ypos);

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        xpos_term,
        ypos_term
    );
    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_cursor_position,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->cursor_position_handler, result);
}

static ERL_NIF_TERM nif_cursor_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->cursor_position_handler);
}
static ERL_NIF_TERM glfw_set_cursor_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->cursor_position_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->cursor_position_handler.set) {
        glfwSetCursorPosCallback(window, cursor_position_callback);
    } else {
        glfwSetCursorPosCallback(window, NULL);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_cursor_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_cursor_position_handler, env, argc, argv);
}

void cursor_enter_callback(GLFWwindow *window, int entered) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_cursor_enter,
        window_resource->window_term,
        entered ? atom_true : atom_false
    );

    beam_handler_send(window_resource->env, &window_resource->cursor_enter_handler, result);
}

static ERL_NIF_TERM nif_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->cursor_enter_handler);
}
static ERL_NIF_TERM glfw_set_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->cursor_enter_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->cursor_enter_handler.set) {
        glfwSetCursorEnterCallback(window, cursor_enter_callback);
    } else {
        glfwSetCursorEnterCallback(window, NULL);
    }

    return atom_ok;
}
static ERL_NIF_TERM nif_set_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_cursor_enter_handler, env, argc, argv);
}

void scroll_callback(GLFWwindow *window, double xoffset, double yoffset) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);

    ERL_NIF_TERM xoffset_term = enif_make_double(window_resource->env, xoffset);
    ERL_NIF_TERM yoffset_term = enif_make_double(window_resource->env, yoffset);

    ERL_NIF_TERM inner_result = enif_make_tuple2(
        window_resource->env,
        xoffset_term,
        yoffset_term
    );
    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_scroll,
        window_resource->window_term,
        inner_result
    );

    beam_handler_send(window_resource->env, &window_resource->scroll_handler, result);
}

static ERL_NIF_TERM nif_scroll_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->scroll_handler);
}
static ERL_NIF_TERM glfw_set_scroll_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    if (!beam_handler_from_term(env, argv[1], &window_resource->scroll_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->scroll_handler.set) {
        glfwSetScrollCallback(window, scroll_callback);
    } else {
        glfwSetScrollCallback(window, NULL);
    }

    return atom_ok;
}
static ERL_NIF_TERM nif_set_scroll_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_scroll_handler, env, argc, argv);
}

void drop_callback(GLFWwindow *window, int path_count, const char *paths[]) {
    GLFWWindowResource* window_resource = glfwGetWindowUserPointer(window);
    if (!window_resource->alive || !window_resource->drop_handler.set) {
        return;
    }

    ERL_NIF_TERM list = enif_make_list(window_resource->env, 0);
    for (int i = path_count - 1; i >= 0; i--) {
        list = enif_make_list_cell(
            window_resource->env,
            enif_make_string(window_resource->env, paths[i], ERL_NIF_UTF8),
            list
        );
    }
    ERL_NIF_TERM result = enif_make_tuple3(
        window_resource->env,
        atom_glfw_drop,
        window_resource->window_term,
        list
    );
    beam_handler_send(window_resource->env, &window_resource->drop_handler, result);
}

static ERL_NIF_TERM nif_drop_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    return beam_handler_to_term(env, &window_resource->drop_handler);
}

static ERL_NIF_TERM glfw_set_drop_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;
    if (!beam_handler_from_term(env, argv[1], &window_resource->drop_handler)) {
        return enif_make_badarg(env);
    }
    if (window_resource->drop_handler.set) {
        glfwSetDropCallback(window, drop_callback);
    } else {
        glfwSetDropCallback(window, NULL);
    }
    return atom_ok;
}

static ERL_NIF_TERM nif_set_drop_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_drop_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_present(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int joy;
    if (!enif_get_int(env, argv[0], &joy)) {
        return enif_make_badarg(env);
    }

    int result = glfwJoystickPresent(joy);
    if (result == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM nif_joystick_present(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_present, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_axes_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    int count;
    const float* axes = glfwGetJoystickAxes(jid, &count);

    if (axes == NULL) {
        return atom_not_present;
    }

    ERL_NIF_TERM axes_list = enif_make_list(env, 0);

    for (int i = count - 1; i >= 0; i--) {
        axes_list = enif_make_list_cell(env,
            enif_make_double(env, (double)axes[i]),
            axes_list
        );
    }

    return axes_list;
}

static ERL_NIF_TERM nif_joystick_axes_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_axes_raw, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_buttons_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    int count;
    const unsigned char* buttons = glfwGetJoystickButtons(jid, &count);

    if (buttons == NULL) {
        return atom_not_present;
    }

    ERL_NIF_TERM buttons_list = enif_make_list(env, 0);

    for (int i = count - 1; i >= 0; i--) {
        ERL_NIF_TERM button_state;
        if (buttons[i] == GLFW_PRESS) {
            button_state = atom_press;
        } else {
            button_state = atom_release;
        }
        buttons_list = enif_make_list_cell(env, button_state, buttons_list);
    }

    return buttons_list;
}

static ERL_NIF_TERM nif_joystick_buttons_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_buttons_raw, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_hats_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    int count;
    const unsigned char* hats = glfwGetJoystickHats(jid, &count);

    if (hats == NULL) {
        return atom_not_present;
    }

    ERL_NIF_TERM hats_list = enif_make_list(env, 0);

    for (int i = count - 1; i >= 0; i--) {
        ERL_NIF_TERM hat_state = enif_make_int(env, hats[i]);
        hats_list = enif_make_list_cell(env, hat_state, hats_list);
    }

    return hats_list;
}

static ERL_NIF_TERM nif_joystick_hats_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_hats_raw, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_name_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    const char* name = glfwGetJoystickName(jid);
    if (name == NULL) {
        return atom_not_present;
    }

    return enif_make_string(env, name, ERL_NIF_UTF8);
}

static ERL_NIF_TERM nif_joystick_name_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_name_raw, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_guid_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    const char* guid = glfwGetJoystickGUID(jid);
    if (guid == NULL) {
        return atom_not_present;
    }

    return enif_make_string(env, guid, ERL_NIF_UTF8);
}

static ERL_NIF_TERM nif_joystick_guid_raw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_guid_raw, env, argc, argv);
}

void joystick_callback(int jid, int event) {
    ERL_NIF_TERM result = enif_make_tuple3(
        glfw_joystick_handler_env,
        atom_glfw_joystick,
        atom_joysticks[jid],
        event == GLFW_CONNECTED ? atom_connected : atom_disconnected
    );

    beam_handler_send(glfw_joystick_handler_env, &glfw_joystick_handler, result);
}

static ERL_NIF_TERM nif_joystick_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    return beam_handler_to_term(env, &glfw_joystick_handler);
}

static ERL_NIF_TERM glfw_set_joystick_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (!beam_handler_from_term(env, argv[0], &glfw_joystick_handler)) {
        return enif_make_badarg(env);
    }
    if (glfw_joystick_handler.set) {
        glfwSetJoystickCallback(joystick_callback);
    } else {
        glfwSetJoystickCallback(NULL);
    }
    return atom_ok;
}

static ERL_NIF_TERM nif_set_joystick_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_joystick_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_is_gamepad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int joy;
    if (!enif_get_int(env, argv[0], &joy)) {
        return enif_make_badarg(env);
    }

    int result = glfwJoystickIsGamepad(joy);
    if (result == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM nif_joystick_is_gamepad(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_joystick_is_gamepad, env, argc, argv);
}

static ERL_NIF_TERM glfw_update_gamepad_mappings(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // Most SDL mappings are under 4KB.
    char mapping[4096];
    if (!enif_get_string(env, argv[0], mapping, sizeof(mapping), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    int result = glfwUpdateGamepadMappings(mapping);
    return (result == GLFW_TRUE) ? atom_true : atom_false;
}

static ERL_NIF_TERM nif_update_gamepad_mappings(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_update_gamepad_mappings, env, argc, argv);
}

static ERL_NIF_TERM glfw_gamepad_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    const char* name = glfwGetGamepadName(jid);
    if (name == NULL) {
        return atom_error;
    }

    return enif_make_tuple2(
        env,
        atom_ok,
        enif_make_string(env, name, ERL_NIF_UTF8)
    );
}

static ERL_NIF_TERM nif_gamepad_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_gamepad_name, env, argc, argv);
}

static ERL_NIF_TERM glfw_gamepad_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int jid;
    if (!enif_get_int(env, argv[0], &jid)) {
        return enif_make_badarg(env);
    }

    GLFWgamepadstate state;
    if (!glfwGetGamepadState(jid, &state)) {
        return atom_error;
    }

    // Create axes map
    ERL_NIF_TERM axes_map = enif_make_new_map(env);
    enif_make_map_put(env, axes_map, atom_axe_left_x, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_LEFT_X]), &axes_map);
    enif_make_map_put(env, axes_map, atom_axe_left_y, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_LEFT_Y]), &axes_map);
    enif_make_map_put(env, axes_map, atom_axe_right_x, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_RIGHT_X]), &axes_map);
    enif_make_map_put(env, axes_map, atom_axe_right_y, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_RIGHT_Y]), &axes_map);
    enif_make_map_put(env, axes_map, atom_axe_left_trigger, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_LEFT_TRIGGER]), &axes_map);
    enif_make_map_put(env, axes_map, atom_axe_right_trigger, enif_make_double(env, state.axes[GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER]), &axes_map);

    // Create buttons map
    ERL_NIF_TERM buttons_map = enif_make_new_map(env);
    enif_make_map_put(env, buttons_map, atom_button_a, state.buttons[GLFW_GAMEPAD_BUTTON_A] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_b, state.buttons[GLFW_GAMEPAD_BUTTON_B] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_x, state.buttons[GLFW_GAMEPAD_BUTTON_X] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_y, state.buttons[GLFW_GAMEPAD_BUTTON_Y] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_left_bumper, state.buttons[GLFW_GAMEPAD_BUTTON_LEFT_BUMPER] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_right_bumper, state.buttons[GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_back, state.buttons[GLFW_GAMEPAD_BUTTON_BACK] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_start, state.buttons[GLFW_GAMEPAD_BUTTON_START] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_guide, state.buttons[GLFW_GAMEPAD_BUTTON_GUIDE] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_left_thumb, state.buttons[GLFW_GAMEPAD_BUTTON_LEFT_THUMB] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_right_thumb, state.buttons[GLFW_GAMEPAD_BUTTON_RIGHT_THUMB] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_dpad_up, state.buttons[GLFW_GAMEPAD_BUTTON_DPAD_UP] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_dpad_right, state.buttons[GLFW_GAMEPAD_BUTTON_DPAD_RIGHT] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_dpad_down, state.buttons[GLFW_GAMEPAD_BUTTON_DPAD_DOWN] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);
    enif_make_map_put(env, buttons_map, atom_button_dpad_left, state.buttons[GLFW_GAMEPAD_BUTTON_DPAD_LEFT] == GLFW_PRESS ? atom_press : atom_release, &buttons_map);

    return enif_make_tuple3(
        env,
        atom_ok,
        axes_map,
        buttons_map
    );
}

static ERL_NIF_TERM nif_gamepad_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_gamepad_state, env, argc, argv);
}

static ERL_NIF_TERM glfw_clipboard_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (enif_is_identical(argv[0], atom_undefined)) {
        const char* string = glfwGetClipboardString(NULL);
        if (string == NULL) {
            return atom_error;
        }
        return enif_make_tuple2(
            env,
            atom_ok,
            enif_make_string(env, string, ERL_NIF_UTF8)
        );
    } else {
        GLFWWindowResource* window_resource;
        if (!beam_get_window(env, argv[0], &window_resource)) {
            return enif_make_badarg(env);
        }
        GLFWwindow* window = window_resource->window;

        const char* string = glfwGetClipboardString(window);
        if (string == NULL) {
            return atom_error;
        }
        return enif_make_tuple2(
            env,
            atom_ok,
            enif_make_string(env, string, ERL_NIF_UTF8)
        );
    }
}

static ERL_NIF_TERM nif_clipboard_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_clipboard_string, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_clipboard_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    char* string = beam_alloc_utf8(env, argv[1]);
    if (string == NULL) {
        return enif_make_badarg(env);
    }

    if (enif_is_identical(argv[0], atom_undefined)) {
        glfwSetClipboardString(NULL, string);
        free(string);
        return atom_ok;
    } else {
        GLFWWindowResource* window_resource;
        if (!beam_get_window(env, argv[0], &window_resource)) {
            free(string);
            return enif_make_badarg(env);
        }
        GLFWwindow* window = window_resource->window;

        glfwSetClipboardString(window, string);
        free(string);
        return atom_ok;
    }
}

static ERL_NIF_TERM nif_set_clipboard_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_clipboard_string, env, argc, argv);
}

#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
static EGLNativeWindowType beam_glfw_wayland_egl_handle(GLFWWindowResource* window_resource, int* ok)
{
    GLFWwindow* window = window_resource->window;
    struct wl_surface* surface = glfwGetWaylandWindow(window);
    if (surface == NULL) {
        *ok = 0;
        return 0;
    }
    if (window_resource->wayland_egl_window == NULL) {
        int width = 0;
        int height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        if (width < 1) {
            width = 1;
        }
        if (height < 1) {
            height = 1;
        }
        window_resource->wayland_egl_window = wl_egl_window_create(surface, width, height);
    }
    if (window_resource->wayland_egl_window == NULL) {
        *ok = 0;
        return 0;
    }
    *ok = 1;
    return (EGLNativeWindowType)(uintptr_t)window_resource->wayland_egl_window;
}
#endif

static EGLNativeWindowType beam_glfw_native_window_handle(GLFWWindowResource* window_resource, int* ok)
{
    GLFWwindow* window = window_resource->window;
    *ok = 0;

#if defined(_WIN32)
    HWND hwnd = glfwGetWin32Window(window);
    if (hwnd == NULL) {
        return 0;
    }
    *ok = 1;
    return (EGLNativeWindowType)(uintptr_t)hwnd;
#elif defined(__APPLE__)
    void* ns_window = glfwGetCocoaWindow(window);
    if (ns_window == NULL) {
        return 0;
    }
    *ok = 1;
    return (EGLNativeWindowType)(uintptr_t)ns_window;
#else
#if defined(BEAM_GLFW_HAS_GET_PLATFORM)
    switch (glfwGetPlatform()) {
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    case GLFW_PLATFORM_WAYLAND:
        return beam_glfw_wayland_egl_handle(window_resource, ok);
#endif
#if defined(BEAM_GLFW_HAS_NATIVE_X11)
    case GLFW_PLATFORM_X11: {
        Window x11_window = glfwGetX11Window(window);
        if (x11_window == None) {
            return 0;
        }
        *ok = 1;
        return (EGLNativeWindowType)x11_window;
    }
#endif
    default:
        return 0;
    }
#else
#if defined(BEAM_GLFW_HAS_NATIVE_WAYLAND)
    {
        EGLNativeWindowType handle = beam_glfw_wayland_egl_handle(window_resource, ok);
        if (*ok) {
            return handle;
        }
    }
#endif
#if defined(BEAM_GLFW_HAS_NATIVE_X11)
    {
        Window x11_window = glfwGetX11Window(window);
        if (x11_window != None) {
            *ok = 1;
            return (EGLNativeWindowType)x11_window;
        }
    }
#endif
    return 0;
#endif
#endif
}

static ERL_NIF_TERM glfw_window_egl_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    GLFWWindowResource* window_resource;
    if (!beam_get_window(env, argv[0], &window_resource)) {
        return enif_make_badarg(env);
    }

    int ok = 0;
    EGLNativeWindowType window_handle = beam_glfw_native_window_handle(window_resource, &ok);
    if (!ok) {
        return atom_error;
    }

    void* egl_window_resource = enif_alloc_resource(egl_window_resource_type, sizeof(EGLNativeWindowType));
    if (!egl_window_resource) {
        return enif_make_atom(env, "allocation_failed");
    }

    *((EGLNativeWindowType*)egl_window_resource) = window_handle;

    ERL_NIF_TERM resource_term = enif_make_resource(env, egl_window_resource);
    enif_release_resource(egl_window_resource);
    return resource_term;
}

static ERL_NIF_TERM nif_window_egl_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_egl_handle, env, argc, argv);
}

static ErlNifFunc nif_functions[] = {
    {"init_hint_raw", 2, nif_init_hint, 0},
    {"init", 0, nif_init_, 0},
    {"terminate", 0, nif_terminate, 0},
    {"version", 0, nif_version, 0},
    {"version_string", 0, nif_version_string, 0},
    {"get_error_raw", 0, nif_get_error, 0},
    {"error_handler", 0, nif_error_handler, 0},
    {"set_error_handler", 1, nif_set_error_handler, 0},
    {"platform_raw", 0, nif_platform, 0},
    {"platform_supported_raw", 1, nif_platform_supported, 0},

    {"monitors", 0, nif_monitors, 0},
    {"primary_monitor", 0, nif_primary_monitor, 0},
    {"monitor_position", 1, nif_monitor_position, 0},
    {"monitor_work_area", 1, nif_monitor_work_area, 0},
    {"monitor_physical_size", 1, nif_monitor_physical_size, 0},
    {"monitor_content_scale", 1, nif_monitor_content_scale, 0},
    {"monitor_name", 1, nif_monitor_name, 0},
    {"monitor_handler", 0, nif_monitor_handler, 0},
    {"monitor_set_handler", 1, nif_monitor_set_handler, 0},
    {"video_modes", 1, nif_video_modes, 0},
    {"video_mode", 1, nif_video_mode, 0},
    {"set_gamma", 2, nif_set_gamma, 0},
    {"gamma_ramp", 1, nif_gamma_ramp, 0},
    {"set_gamma_ramp", 2, nif_set_gamma_ramp, 0},

    {"default_window_hints", 0, nif_default_window_hints, 0},
    {"window_hint_raw", 2, nif_window_hint, 0},
    {"window_hint_string_raw", 2, nif_window_hint_string, 0},

    {"create_window", 3, nif_create_window, 0},
    {"destroy_window", 1, nif_destroy_window, 0},
    {"window_should_close", 1, nif_window_should_close, 0},
    {"set_window_should_close", 2, nif_set_window_should_close, 0},
    {"window_title", 1, nif_window_title, 0},
    {"set_window_title", 2, nif_set_window_title, 0},
    {"set_window_icon", 2, nif_set_window_icon, 0},
    {"window_position", 1, nif_window_position, 0},
    {"set_window_position", 2, nif_set_window_position, 0},
    {"window_size", 1, nif_window_size, 0},
    {"set_window_size", 2, nif_set_window_size, 0},
    {"framebuffer_size", 1, nif_framebuffer_size, 0},
    {"set_window_size_limits_raw", 5, nif_set_window_size_limits, 0},
    {"set_window_aspect_ratio_raw", 3, nif_set_window_aspect_ratio, 0},
    {"window_frame_size", 1, nif_window_frame_size, 0},
    {"window_content_scale", 1, nif_window_content_scale, 0},
    {"window_opacity", 1, nif_window_opacity, 0},
    {"set_window_opacity", 2, nif_set_window_opacity, 0},
    {"iconify_window", 1, nif_iconify_window, 0},
    {"restore_window", 1, nif_restore_window, 0},
    {"maximize_window", 1, nif_maximize_window, 0},
    {"show_window", 1, nif_show_window, 0},
    {"hide_window", 1, nif_hide_window, 0},
    {"focus_window", 1, nif_focus_window, 0},
    {"request_window_attention", 1, nif_request_window_attention, 0},

    {"window_monitor", 1, nif_window_monitor, 0},
    {"set_window_monitor_raw", 7, nif_set_window_monitor, 0},

    {"window_attrib_raw", 2, nif_window_attrib, 0},
    {"set_window_attrib_raw", 3, nif_set_window_attrib, 0},

    {"window_position_handler", 1, nif_window_position_handler, 0},
    {"set_window_position_handler", 2, nif_set_window_position_handler, 0},
    {"window_size_handler", 1, nif_window_size_handler, 0},
    {"set_window_size_handler", 2, nif_set_window_size_handler, 0},
    {"window_close_handler", 1, nif_window_close_handler, 0},
    {"set_window_close_handler", 2, nif_set_window_close_handler, 0},
    {"window_refresh_handler", 1, nif_window_refresh_handler, 0},
    {"set_window_refresh_handler", 2, nif_set_window_refresh_handler, 0},
    {"window_focus_handler", 1, nif_window_focus_handler, 0},
    {"set_window_focus_handler", 2, nif_set_window_focus_handler, 0},
    {"window_iconify_handler", 1, nif_window_iconify_handler, 0},
    {"set_window_iconify_handler", 2, nif_set_window_iconify_handler, 0},
    {"window_maximize_handler", 1, nif_window_maximize_handler, 0},
    {"set_window_maximize_handler", 2, nif_set_window_maximize_handler, 0},
    {"window_content_scale_handler", 1, nif_window_content_scale_handler, 0},
    {"set_window_content_scale_handler", 2, nif_set_window_content_scale_handler, 0},
    {"framebuffer_size_handler", 1, nif_framebuffer_size_handler, 0},
    {"set_framebuffer_size_handler", 2, nif_set_framebuffer_size_handler, 0},

    {"poll_events", 0, nif_poll_events, 0},
    {"post_empty_event", 0, nif_post_empty_event, 0},

    {"input_mode_raw", 2, nif_input_mode, 0},
    {"set_input_mode_raw", 3, nif_set_input_mode, 0},

    {"create_cursor_raw", 5, nif_create_cursor, 0},
    {"create_standard_cursor_raw", 1, nif_create_standard_cursor, 0},
    {"destroy_cursor", 1, nif_destroy_cursor, 0},
    {"set_cursor", 2, nif_set_cursor, 0},

    {"raw_mouse_motion_supported", 0, nif_raw_mouse_motion_supported, 0},
    {"key_name_key", 1, nif_key_name_key, 0},
    {"key_name_scancode", 1, nif_key_name_scancode, 0},
    {"key_scancode_raw", 1, nif_key_scancode, 0},
    {"key_raw", 2, nif_key, 0},
    {"mouse_button_raw", 2, nif_mouse_button, 0},

    {"cursor_position", 1, nif_cursor_position, 0},
    {"set_cursor_position_raw", 3, nif_set_cursor_position, 0},

    {"key_handler", 1, nif_key_handler, 0},
    {"set_key_handler", 2, nif_set_key_handler, 0},
    {"char_handler", 1, nif_character_handler, 0},
    {"set_char_handler", 2, nif_set_character_handler, 0},
    {"char_mods_handler", 1, nif_character_mods_handler, 0},
    {"set_char_mods_handler", 2, nif_set_character_mods_handler, 0},
    {"mouse_button_handler", 1, nif_mouse_button_handler, 0},
    {"set_mouse_button_handler", 2, nif_set_mouse_button_handler, 0},
    {"cursor_position_handler", 1, nif_cursor_position_handler, 0},
    {"set_cursor_position_handler", 2, nif_set_cursor_position_handler, 0},
    {"cursor_enter_handler", 1, nif_cursor_enter_handler, 0},
    {"set_cursor_enter_handler", 2, nif_set_cursor_enter_handler, 0},
    {"scroll_handler", 1, nif_scroll_handler, 0},
    {"set_scroll_handler", 2, nif_set_scroll_handler, 0},
    {"drop_handler", 1, nif_drop_handler, 0},
    {"set_drop_handler", 2, nif_set_drop_handler, 0},

    {"joystick_present_raw", 1, nif_joystick_present, 0},

    {"joystick_axes_raw", 1, nif_joystick_axes_raw, 0},
    {"joystick_buttons_raw", 1, nif_joystick_buttons_raw, 0},
    {"joystick_hats_raw", 1, nif_joystick_hats_raw, 0},

    {"joystick_name_raw", 1, nif_joystick_name_raw, 0},
    {"joystick_guid_raw", 1, nif_joystick_guid_raw, 0},

    {"joystick_handler", 0, nif_joystick_handler, 0},
    {"set_joystick_handler", 1, nif_set_joystick_handler, 0},

    {"joystick_is_gamepad_raw", 1, nif_joystick_is_gamepad, 0},
    {"update_gamepad_mappings", 1, nif_update_gamepad_mappings, 0},

    {"gamepad_name_raw", 1, nif_gamepad_name, 0},
    {"gamepad_state_raw", 1, nif_gamepad_state, 0},

    {"clipboard_string", 1, nif_clipboard_string, 0},
    {"set_clipboard_string", 2, nif_set_clipboard_string, 0},

    {"window_egl_handle", 1, nif_window_egl_handle, 0}
};

ERL_NIF_INIT(
    glfw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
