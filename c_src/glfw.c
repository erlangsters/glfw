//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project directory.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
//
#include <string.h>
#include <pthread.h>
#include <dlfcn.h>
#include <erl_nif.h>
#include <EGL/egl.h>
#include <GLFW/glfw3.h>

static void* egl_nif_lib_handle = NULL;
ErlNifResourceType* egl_window_resource_type;

static pthread_t commands_executor;
static pthread_mutex_t command_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t command_ready = PTHREAD_COND_INITIALIZER;
static pthread_cond_t command_done = PTHREAD_COND_INITIALIZER;

static ERL_NIF_TERM (*command_function)(ErlNifEnv*, int, const ERL_NIF_TERM[]) = NULL;
static ErlNifEnv* command_args_1 = NULL;
static int command_args_2 = 0;
static ERL_NIF_TERM** command_args_3 = NULL;
static ERL_NIF_TERM command_result;

static int command_finished = 0;

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

static ERL_NIF_TERM atom_glfw_key;
static ERL_NIF_TERM atom_glfw_char;
static ERL_NIF_TERM atom_glfw_char_mods;
static ERL_NIF_TERM atom_glfw_mouse_button;
static ERL_NIF_TERM atom_glfw_cursor_position;
static ERL_NIF_TERM atom_glfw_cursor_enter;

static ERL_NIF_TERM atom_press;
static ERL_NIF_TERM atom_release;

static ErlNifResourceType* glfw_monitor_resource_type = NULL;
static ErlNifResourceType* glfw_window_resource_type = NULL;
static ErlNifResourceType* glfw_cursor_resource_type = NULL;

static ErlNifEnv* glfw_error_handler_env = NULL;
static ERL_NIF_TERM glfw_error_handler;

typedef struct {
    ErlNifEnv* env;
    GLFWwindow* window;
    ERL_NIF_TERM window_term;
    ERL_NIF_TERM window_position_handler;
    ERL_NIF_TERM window_size_handler;
    ERL_NIF_TERM window_close_handler;
    ERL_NIF_TERM window_refresh_handler;
    ERL_NIF_TERM window_focus_handler;
    ERL_NIF_TERM window_iconify_handler;
    ERL_NIF_TERM window_maximize_handler;
    ERL_NIF_TERM window_content_scale_handler;
    ERL_NIF_TERM key_handler;
    ERL_NIF_TERM char_handler;
    ERL_NIF_TERM char_mods_handler;
    ERL_NIF_TERM mouse_button_handler;
    ERL_NIF_TERM cursor_position_handler;
    ERL_NIF_TERM cursor_enter_handler;
} GLFWWindowResource;

// The function of the thread that executes "NIF commands". It just waits for a
// command to be ready, executes it and signals that the command is done (while
// making the result available).
void* commands_executor_function(void* arg) {
    // XXX: Improve the implementation. For now it does the job.
    while (1) {
        pthread_mutex_lock(&command_mutex);
        while (command_function == NULL) {
            pthread_cond_wait(&command_ready, &command_mutex);
        }

        command_result = command_function(
            command_args_1,
            command_args_2,
            (const ERL_NIF_TERM**)command_args_3
        );

        command_function = NULL;
        command_finished = 1;
        pthread_cond_signal(&command_done);
        pthread_mutex_unlock(&command_mutex);
    }

    return NULL;
}

// It executes a "NIF command". It schedules the command to be executed by the
// commands executor thread, waits for the command to be executed and returns
// the result of the command.
ERL_NIF_TERM execute_command(
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[]
) {
    pthread_mutex_lock(&command_mutex);
    command_function = function;
    command_args_1 = env;
    command_args_2 = argc;
    command_args_3 = argv;
    command_finished = 0;

    pthread_cond_signal(&command_ready);

    while (!command_finished) {
        pthread_cond_wait(&command_done, &command_mutex);
    }

    ERL_NIF_TERM result = command_result;
    pthread_mutex_unlock(&command_mutex);

    return result;
}

static void glfw_monitor_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void glfw_window_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void glfw_cursor_resource_dtor(ErlNifEnv* env, void* obj) {
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    char beam_egl_so_path[1024];
    if (!enif_get_string(env, arg, beam_egl_so_path, sizeof(beam_egl_so_path), ERL_NIF_LATIN1)) {
        fprintf(stderr, "failed to read EGL binding library path from argument\n");
        return -1;
    }

    egl_nif_lib_handle = dlopen(beam_egl_so_path, RTLD_NOW);
    if (!egl_nif_lib_handle) {
        fprintf(stderr, "failed to load beam-egl.so: %s\n", dlerror());
        return -1;
    }

    egl_window_resource_type = dlsym(egl_nif_lib_handle, "egl_window_resource_type");
    if (!egl_window_resource_type) {
        fprintf(stderr, "failed to load symbol egl_window_resource_type: %s\n", dlerror());
        dlclose(egl_nif_lib_handle);
        return -1;
    }

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

    atom_glfw_key = enif_make_atom(env, "glfw_key");
    atom_glfw_char = enif_make_atom(env, "glfw_char");
    atom_glfw_char_mods = enif_make_atom(env, "glfw_char_mods");
    atom_glfw_mouse_button = enif_make_atom(env, "glfw_mouse_button");
    atom_glfw_cursor_position = enif_make_atom(env, "glfw_cursor_position");
    atom_glfw_cursor_enter = enif_make_atom(env, "glfw_cursor_enter");

    atom_press = enif_make_atom(env, "press");
    atom_release = enif_make_atom(env, "release");

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
    glfw_error_handler = enif_make_copy(glfw_error_handler_env, atom_undefined);

    // Start the "NIF commands" executor thread.
    if (pthread_create(&commands_executor, NULL, commands_executor_function, NULL) != 0) {
        fprintf(stderr, "failed to create the commands executor thread\n");
        return -1;
    }

    return 0;
}

static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)
{
    enif_free_env(glfw_error_handler_env);

    return 0;
}

static ERL_NIF_TERM glfw_init_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
    int result = glfwInit();
    if (result == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM nif_init_(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_init_command, env, argc, argv);
}

static ERL_NIF_TERM glfw_terminate_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwTerminate();
    return atom_ok;
}

static ERL_NIF_TERM nif_terminate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_terminate_command, env, argc, argv);
}

static ERL_NIF_TERM nif_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    const char* version = glfwGetVersionString();
    return enif_make_string(env, version, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
    ERL_NIF_TERM code_term;
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
        case GLFW_API_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "api_unavailable");
            break;
        case GLFW_VERSION_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "version_unavailable");
            break;
        case GLFW_PLATFORM_ERROR:
            code_term = enif_make_atom(glfw_error_handler_env, "platform_error");
            break;
        case GLFW_FORMAT_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "format_unavailable");
            break;
        case GLFW_NO_WINDOW_CONTEXT:
            code_term = enif_make_atom(glfw_error_handler_env, "no_window_context");
            break;
        case GLFW_CURSOR_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "cursor_unavailable");
            break;
        case GLFW_FEATURE_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "feature_unavailable");
            break;
        case GLFW_FEATURE_UNIMPLEMENTED:
            code_term = enif_make_atom(glfw_error_handler_env, "feature_unimplemented");
            break;
        case GLFW_PLATFORM_UNAVAILABLE:
            code_term = enif_make_atom(glfw_error_handler_env, "platform_unavailable");
            break;
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

    enif_send(NULL, &glfw_error_handler, NULL, result);
}

static ERL_NIF_TERM nif_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return glfw_error_handler;
}

static ERL_NIF_TERM glfw_set_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enif_clear_env(glfw_error_handler_env);

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetErrorCallback(NULL);
        glfw_error_handler = enif_make_copy(glfw_error_handler_env, atom_undefined);
    } else {
        glfwSetErrorCallback(error_callback);
        glfw_error_handler = enif_make_copy(glfw_error_handler_env, argv[0]);
    }

    return atom_ok;
}

static ERL_NIF_TERM nif_set_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_error_handler, env, argc, argv);
}

static ERL_NIF_TERM nif_platform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    int platform = glfwGetPlatform();
    return enif_make_int(env, platform);
}

static ERL_NIF_TERM nif_platform_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    int platform;
    if (!enif_get_int(env, argv[0], &platform)) {
        return enif_make_badarg(env);
    }

    int supported = glfwPlatformSupported(platform);
    if (supported == GLFW_TRUE) {
        return atom_true;
    }
    else {
        return atom_false;
    }
}

static ERL_NIF_TERM glfw_monitors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int count;
    GLFWmonitor** monitors = glfwGetMonitors(&count);
    if (!monitors) {
        return enif_make_list(env, 0);
    }

    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (int i = count - 1; i >= 0; i--) {
        void* monitor_resource = enif_alloc_resource(glfw_monitor_resource_type, sizeof(GLFWmonitor*));
        *((GLFWmonitor**)monitor_resource) = monitors[i];

        ERL_NIF_TERM monitor_term = enif_make_resource(env, monitor_resource);
        enif_release_resource(monitor_resource);

        list = enif_make_list_cell(env, monitor_term, list);
    }

    return list;
}

static ERL_NIF_TERM nif_monitors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitors, env, argc, argv);
}

static ERL_NIF_TERM glfw_primary_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor* monitor = glfwGetPrimaryMonitor();
    if (!monitor) {
        return atom_no_monitor;
    }

    void* monitor_resource = enif_alloc_resource(glfw_monitor_resource_type, sizeof(GLFWmonitor*));
    *((GLFWmonitor**)monitor_resource) = monitor;

    ERL_NIF_TERM monitor_ref = enif_make_resource(env, monitor_resource);
    enif_release_resource(monitor_resource);

    return enif_make_tuple2(
        env,
        atom_ok,
        monitor_ref
    );
}

static ERL_NIF_TERM nif_primary_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_primary_monitor, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

    const char* name = glfwGetMonitorName(*monitor);
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_monitor_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_name, env, argc, argv);
}


static ERL_NIF_TERM nif_monitor_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_set_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM glfw_monitor_video_modes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

    int count;
    const GLFWvidmode* modes = glfwGetVideoModes(monitor, &count);
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

static ERL_NIF_TERM nif_monitor_video_modes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_video_modes, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_video_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

    const GLFWvidmode* mode = glfwGetVideoMode(monitor);
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

static ERL_NIF_TERM nif_monitor_video_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_video_mode, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_set_gamma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

    double gamma;
    if (!enif_get_double(env, argv[1], &gamma)) {
        return enif_make_badarg(env);
    }

    glfwSetGamma(*monitor, gamma);
    return atom_ok;
}

static ERL_NIF_TERM nif_monitor_set_gamma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_set_gamma, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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

static ERL_NIF_TERM nif_monitor_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_gamma_ramp, env, argc, argv);
}

static ERL_NIF_TERM glfw_monitor_set_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // XXX: Review implementation.

    GLFWmonitor** monitor;
    if (!enif_get_resource(env, argv[0], glfw_monitor_resource_type, (void**) &monitor)) {
        return enif_make_badarg(env);
    }

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

static ERL_NIF_TERM nif_monitor_set_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_monitor_set_gamma_ramp, env, argc, argv);
}

static ERL_NIF_TERM glfw_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    int width, height;
    char title[256];

    if (!enif_get_int(env, argv[0], &width)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &height)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[2], title, sizeof(title), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    // XXX: This will be removed after window hints are implemented.
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    GLFWwindow* window = glfwCreateWindow(width, height, title, NULL, NULL);
    if (!window) {
        return atom_no_window;
    }

    GLFWWindowResource* window_resource = enif_alloc_resource(
        glfw_window_resource_type,
        sizeof(GLFWWindowResource)
    );
    window_resource->env = enif_alloc_env();
    window_resource->window = window;
    window_resource->window_position_handler = atom_undefined;
    window_resource->window_size_handler = atom_undefined;
    window_resource->window_close_handler = atom_undefined;
    window_resource->window_refresh_handler = atom_undefined;
    window_resource->window_focus_handler = atom_undefined;
    window_resource->window_iconify_handler = atom_undefined;
    window_resource->window_maximize_handler = atom_undefined;
    window_resource->window_content_scale_handler = atom_undefined;

    glfwSetWindowUserPointer(window, window_resource);

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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    glfwDestroyWindow(window);
    return atom_ok;
}

static ERL_NIF_TERM nif_destroy_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_destroy_window, env, argc, argv);
}

static ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    const char* title = glfwGetWindowTitle(window);
    return enif_make_string(env, title, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_title, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    char title[256];
    if (!enif_get_string(env, argv[1], title, sizeof(title), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowTitle(window, title);
    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_title, env, argc, argv);
}

static ERL_NIF_TERM nif_set_window_icon(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM glfw_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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

static ERL_NIF_TERM glfw_set_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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

    enif_send(NULL, &window_resource->window_position_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_position_handler;
}

static ERL_NIF_TERM glfw_set_window_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowPosCallback(window, NULL);
    } else {
        glfwSetWindowPosCallback(window, window_position_callback);
    }
    window_resource->window_position_handler = enif_make_copy(env, argv[1]);

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

    enif_send(NULL, &window_resource->window_size_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_size_handler;
}

static ERL_NIF_TERM glfw_set_window_size_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowSizeCallback(window, NULL);
    } else {
        glfwSetWindowSizeCallback(window, window_size_callback);
    }
    window_resource->window_size_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_close_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_close_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_close_handler;
}

static ERL_NIF_TERM glfw_set_window_close_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowCloseCallback(window, NULL);
    } else {
        glfwSetWindowCloseCallback(window, window_close_callback);
    }
    window_resource->window_close_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_refresh_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_refresh_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_refresh_handler;
}

static ERL_NIF_TERM glfw_set_window_refresh_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowRefreshCallback(window, NULL);
    } else {
        glfwSetWindowRefreshCallback(window, window_refresh_callback);
    }
    window_resource->window_refresh_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_focus_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_focus_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_focus_handler;
}

static ERL_NIF_TERM glfw_set_window_focus_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowFocusCallback(window, NULL);
    } else {
        glfwSetWindowFocusCallback(window, window_focus_callback);
    }
    window_resource->window_focus_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_iconify_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_iconify_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_iconify_handler;
}

static ERL_NIF_TERM glfw_set_window_iconify_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowIconifyCallback(window, NULL);
    } else {
        glfwSetWindowIconifyCallback(window, window_iconify_callback);
    }
    window_resource->window_iconify_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_maximize_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_maximize_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_maximize_handler;
}

static ERL_NIF_TERM glfw_set_window_maximize_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowMaximizeCallback(window, NULL);
    } else {
        glfwSetWindowMaximizeCallback(window, window_maximize_callback);
    }
    window_resource->window_maximize_handler = argv[1];

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

    enif_send(NULL, &window_resource->window_content_scale_handler, NULL, result);
}

static ERL_NIF_TERM nif_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->window_content_scale_handler;
}

static ERL_NIF_TERM glfw_set_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetWindowContentScaleCallback(window, NULL);
    } else {
        glfwSetWindowContentScaleCallback(window, window_content_scale_callback);
    }
    window_resource->window_content_scale_handler = argv[1];

    return atom_ok;
}

static ERL_NIF_TERM nif_set_window_content_scale_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_content_scale_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwPollEvents();
    return atom_ok;
}

static ERL_NIF_TERM nif_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_poll_events, env, argc, argv);
}

static ERL_NIF_TERM glfw_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwPostEmptyEvent();
    return atom_ok;
}

static ERL_NIF_TERM nif_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_post_empty_event, env, argc, argv);
}

static ERL_NIF_TERM glfw_input_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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

    void* cursor_resource = enif_alloc_resource(glfw_cursor_resource_type, sizeof(GLFWcursor*));
    *((GLFWcursor**)cursor_resource) = cursor;

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
    int shape;
    if (!enif_get_int(env, argv[0], &shape)) {
        return enif_make_badarg(env);
    }

    GLFWcursor* cursor = glfwCreateStandardCursor(shape);
    if (!cursor) {
        return atom_error;
    }

    void* cursor_resource = enif_alloc_resource(glfw_cursor_resource_type, sizeof(GLFWcursor*));
    *((GLFWcursor**)cursor_resource) = cursor;

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
    GLFWcursor** cursor;
    if (!enif_get_resource(env, argv[0], glfw_cursor_resource_type, (void**) &cursor)) {
        return enif_make_badarg(env);
    }

    glfwDestroyCursor(*cursor);
    return atom_ok;
}

static ERL_NIF_TERM nif_destroy_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_destroy_cursor, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_cursor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
        GLFWcursor** cursor;
        if (!enif_get_resource(env, argv[1], glfw_cursor_resource_type, (void**) &cursor)) {
            return enif_make_badarg(env);
        }
        cursor_value = *cursor;
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    double xpos, ypos;
    glfwGetCursorPos(*window, &xpos, &ypos);

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
    GLFWwindow** window;
    double xpos, ypos;

    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_double(env, argv[1], &xpos) || !enif_get_double(env, argv[2], &ypos)) {
        return enif_make_badarg(env);
    }

    glfwSetCursorPos(*window, xpos, ypos);
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

    enif_send(NULL, &window_resource->key_handler, NULL, result);
}

static ERL_NIF_TERM nif_key_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->key_handler;
}

static ERL_NIF_TERM glfw_set_key_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetKeyCallback(window, NULL);
    } else {
        glfwSetKeyCallback(window, key_callback);
    }
    window_resource->key_handler = argv[1];

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

    enif_send(NULL, &window_resource->char_handler, NULL, result);
}

static ERL_NIF_TERM nif_character_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->char_handler;
}
static ERL_NIF_TERM glfw_set_character_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetCharCallback(window, NULL);
    } else {
        glfwSetCharCallback(window, char_callback);
    }
    window_resource->char_handler = argv[1];

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

    enif_send(NULL, &window_resource->char_mods_handler, NULL, result);
}

static ERL_NIF_TERM nif_character_mods_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->char_mods_handler;
}
static ERL_NIF_TERM glfw_set_character_mods_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetCharModsCallback(window, NULL);
    } else {
        glfwSetCharModsCallback(window, char_mods_callback);
    }
    window_resource->char_mods_handler = argv[1];

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

    enif_send(NULL, &window_resource->mouse_button_handler, NULL, result);
}

static ERL_NIF_TERM nif_mouse_button_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->mouse_button_handler;
}
static ERL_NIF_TERM glfw_set_mouse_button_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetMouseButtonCallback(window, NULL);
    } else {
        glfwSetMouseButtonCallback(window, mouse_button_callback);
    }
    window_resource->mouse_button_handler = argv[1];

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

    enif_send(NULL, &window_resource->cursor_position_handler, NULL, result);
}

static ERL_NIF_TERM nif_cursor_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->cursor_position_handler;
}
static ERL_NIF_TERM glfw_set_cursor_position_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetCursorPosCallback(window, NULL);
    } else {
        glfwSetCursorPosCallback(window, cursor_position_callback);
    }
    window_resource->cursor_position_handler = argv[1];

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

    enif_send(NULL, &window_resource->cursor_enter_handler, NULL, result);
}

static ERL_NIF_TERM nif_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }

    return window_resource->cursor_enter_handler;
}
static ERL_NIF_TERM glfw_set_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    int is_undefined = enif_is_identical(argv[1], atom_undefined);
    if(is_undefined) {
        glfwSetCursorEnterCallback(window, NULL);
    } else {
        glfwSetCursorEnterCallback(window, cursor_enter_callback);
    }
    window_resource->cursor_enter_handler = argv[1];

    return atom_ok;
}
static ERL_NIF_TERM nif_set_cursor_enter_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_cursor_enter_handler, env, argc, argv);
}

static ERL_NIF_TERM glfw_joystick_present(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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

static ERL_NIF_TERM nif_window_egl_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWWindowResource* window_resource;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window_resource)) {
        return enif_make_badarg(env);
    }
    GLFWwindow* window = window_resource->window;

    EGLNativeWindowType window_handle = (EGLNativeWindowType)glfwGetX11Window(window);

    void* egl_window_resource = enif_alloc_resource(egl_window_resource_type, sizeof(EGLNativeWindowType));

    *((EGLNativeWindowType*)egl_window_resource) = window_handle;

    return enif_make_resource(env, egl_window_resource);
}

static ErlNifFunc nif_functions[] = {
    {"init_hint_raw", 2, nif_init_hint},
    {"init", 0, nif_init_},
    {"terminate", 0, nif_terminate},
    {"version", 0, nif_version},
    {"version_string", 0, nif_version_string},
    {"get_error_raw", 0, nif_get_error},
    {"error_handler", 0, nif_error_handler},
    {"set_error_handler", 1, nif_set_error_handler},
    {"platform_raw", 0, nif_platform},
    {"platform_supported_raw", 1, nif_platform_supported},

    {"monitors", 0, nif_monitors},
    {"primary_monitor", 0, nif_primary_monitor},
    {"monitor_position", 1, nif_monitor_position},
    {"monitor_work_area", 1, nif_monitor_work_area},
    {"monitor_physical_size", 1, nif_monitor_physical_size},
    {"monitor_content_scale", 1, nif_monitor_content_scale},
    {"monitor_name", 1, nif_monitor_name},
    {"monitor_handler", 1, nif_monitor_handler},
    {"monitor_set_handler", 2, nif_monitor_set_handler},
    {"monitor_video_modes", 1, nif_monitor_video_modes},
    {"monitor_video_mode", 1, nif_monitor_video_mode},
    {"monitor_set_gamma", 2, nif_monitor_set_gamma},
    {"monitor_gamma_ramp", 1, nif_monitor_gamma_ramp},
    {"monitor_set_gamma_ramp", 2, nif_monitor_set_gamma_ramp},

    {"create_window", 3, nif_create_window},
    {"destroy_window", 1, nif_destroy_window},
    {"window_should_close", 1, nif_window_should_close},
    {"set_window_should_close", 2, nif_set_window_should_close},
    {"window_title", 1, nif_window_title},
    {"set_window_title", 2, nif_set_window_title},
    {"set_window_icon", 2, nif_set_window_icon},
    {"window_position", 1, nif_window_position},
    {"set_window_position", 2, nif_set_window_position},
    {"window_size", 1, nif_window_size},
    {"set_window_size", 2, nif_set_window_size},
    {"set_window_size_limits_raw", 5, nif_set_window_size_limits},
    {"set_window_aspect_ratio_raw", 3, nif_set_window_aspect_ratio},
    {"window_frame_size", 1, nif_window_frame_size},
    {"window_content_scale", 1, nif_window_content_scale},
    {"window_opacity", 1, nif_window_opacity},
    {"set_window_opacity", 2, nif_set_window_opacity},
    {"iconify_window", 1, nif_iconify_window},
    {"restore_window", 1, nif_restore_window},
    {"maximize_window", 1, nif_maximize_window},
    {"show_window", 1, nif_show_window},
    {"hide_window", 1, nif_hide_window},
    {"focus_window", 1, nif_focus_window},
    {"request_window_attention", 1, nif_request_window_attention},

    {"window_position_handler", 1, nif_window_position_handler},
    {"set_window_position_handler", 2, nif_set_window_position_handler},
    {"window_size_handler", 1, nif_window_size_handler},
    {"set_window_size_handler", 2, nif_set_window_size_handler},
    {"window_close_handler", 1, nif_window_close_handler},
    {"set_window_close_handler", 2, nif_set_window_close_handler},
    {"window_refresh_handler", 1, nif_window_refresh_handler},
    {"set_window_refresh_handler", 2, nif_set_window_refresh_handler},
    {"window_focus_handler", 1, nif_window_focus_handler},
    {"set_window_focus_handler", 2, nif_set_window_focus_handler},
    {"window_iconify_handler", 1, nif_window_iconify_handler},
    {"set_window_iconify_handler", 2, nif_set_window_iconify_handler},
    {"window_maximize_handler", 1, nif_window_maximize_handler},
    {"set_window_maximize_handler", 2, nif_set_window_maximize_handler},
    {"window_content_scale_handler", 1, nif_window_content_scale_handler},
    {"set_window_content_scale_handler", 2, nif_set_window_content_scale_handler},

    {"poll_events", 0, nif_poll_events},
    {"post_empty_event", 0, nif_post_empty_event},

    {"input_mode_raw", 2, nif_input_mode},
    {"set_input_mode_raw", 3, nif_set_input_mode},

    {"create_cursor_raw", 5, nif_create_cursor},
    {"create_standard_cursor_raw", 1, nif_create_standard_cursor},
    {"destroy_cursor", 1, nif_destroy_cursor},
    {"set_cursor", 2, nif_set_cursor},

    {"raw_mouse_motion_supported", 0, nif_raw_mouse_motion_supported},
    {"key_name_key", 1, nif_key_name_key},
    {"key_name_scancode", 1, nif_key_name_scancode},
    {"key_scancode_raw", 1, nif_key_scancode},
    {"key_raw", 2, nif_key},
    {"mouse_button_raw", 2, nif_mouse_button},

    {"cursor_position", 1, nif_cursor_position},
    {"set_cursor_position_raw", 3, nif_set_cursor_position},

    {"key_handler", 1, nif_key_handler},
    {"set_key_handler", 2, nif_set_key_handler},
    {"char_handler", 1, nif_character_handler},
    {"set_char_handler", 2, nif_set_character_handler},
    {"char_mods_handler", 1, nif_character_mods_handler},
    {"set_char_mods_handler", 2, nif_set_character_mods_handler},
    {"mouse_button_handler", 1, nif_mouse_button_handler},
    {"set_mouse_button_handler", 2, nif_set_mouse_button_handler},
    {"cursor_position_handler", 1, nif_cursor_position_handler},
    {"set_cursor_position_handler", 2, nif_set_cursor_position_handler},
    {"cursor_enter_handler", 1, nif_cursor_enter_handler},
    {"set_cursor_enter_handler", 2, nif_set_cursor_enter_handler},

    {"joystick_present_raw", 1, nif_joystick_present},

    {"window_egl_handle", 1, nif_window_egl_handle}
};

ERL_NIF_INIT(
    glfw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
