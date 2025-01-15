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
#include <erl_nif.h>
#include <GLFW/glfw3.h>

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

static ErlNifResourceType* glfw_monitor_resource_type = NULL;
static ErlNifResourceType* glfw_window_resource_type = NULL;


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

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
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

    // Start the "NIF commands" executor thread.
    if (pthread_create(&commands_executor, NULL, commands_executor_function, NULL) != 0) {
        fprintf(stderr, "failed to create the commands executor thread\n");
        return -1;
    }

    return 0;
}

static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)
{
    return 0;
}

static ERL_NIF_TERM nif_init_hint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM glfw_init_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int result = glfwInit();
    if (result == GLFW_TRUE) {
        return enif_make_atom(env, "true");
    }
    else {
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM nif_init_(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_init_command, env, argc, argv);
}

static ERL_NIF_TERM glfw_terminate_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwTerminate();
    return enif_make_atom(env, "ok");
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

static ERL_NIF_TERM nif_last_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_set_error_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_platform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_platform_supported(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
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
        return enif_make_atom(env, "no_monitor");
    }

    void* monitor_resource = enif_alloc_resource(glfw_monitor_resource_type, sizeof(GLFWmonitor*));
    *((GLFWmonitor**)monitor_resource) = monitor;

    ERL_NIF_TERM monitor_ref = enif_make_resource(env, monitor_resource);
    enif_release_resource(monitor_resource);

    return enif_make_tuple2(
        env,
        enif_make_atom(env, "ok"),
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
        return enif_make_atom(env, "undefined");
    }

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = count - 1; i >= 0; i--) {
        ERL_NIF_TERM mode = enif_make_tuple7(
            env,
            enif_make_atom(env, "glfw_video_mode"),
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
        return enif_make_atom(env, "undefined");
    }

    return enif_make_tuple7(env,
        enif_make_atom(env, "glfw_video_mode"),
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
    return enif_make_atom(env, "ok");
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
        return enif_make_atom(env, "undefined");
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
        enif_make_atom(env, "glfw_gamma_ramp"),
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
    return enif_make_atom(env, "ok");

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
        return enif_make_atom(env, "no_window");
    }

    void* window_resource = enif_alloc_resource(glfw_window_resource_type, sizeof(GLFWwindow*));
    *((GLFWwindow**)window_resource) = window;

    ERL_NIF_TERM window_ref = enif_make_resource(env, window_resource);
    enif_release_resource(window_resource);

    return enif_make_tuple2(
        env,
        enif_make_atom(env, "ok"),
        window_ref
    );
}

static ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_create_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_destroy_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwDestroyWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_destroy_window, env, argc, argv);
}

static ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int result = glfwWindowShouldClose(*window);
    if (result == GLFW_TRUE) {
        return enif_make_atom(env, "true");
    }
    else {
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM nif_set_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // According to the doc, this function can be called from any thread (no
    // need to use the NIF function executor thread).
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int value;
    if (enif_is_identical(argv[1], enif_make_atom(env, "true"))) {
        value = GLFW_TRUE;
    } else if (enif_is_identical(argv[1], enif_make_atom(env, "false"))) {
        value = GLFW_FALSE;
    } else {
        return enif_make_badarg(env);
    }

    glfwSetWindowShouldClose(*window, value);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM glfw_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    const char* title = glfwGetWindowTitle(*window);
    return enif_make_string(env, title, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_title, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_title(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    char title[256];
    if (!enif_get_string(env, argv[1], title, sizeof(title), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowTitle(*window, title);
    return enif_make_atom(env, "ok");
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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int x, y;
    glfwGetWindowPos(*window, &x, &y);

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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    const ERL_NIF_TERM* xy;
    int arity;
    if (!enif_get_tuple(env, argv[1], &arity, &xy) || arity != 2) {
        return enif_make_badarg(env);
    }

    int x, y;
    if (!enif_get_int(env, xy[0], &x) || !enif_get_int(env, xy[1], &y)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowPos(*window, x, y);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_set_window_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_position, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int width, height;
    glfwGetWindowSize(*window, &width, &height);

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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    const ERL_NIF_TERM* wh;
    int arity;
    if (!enif_get_tuple(env, argv[1], &arity, &wh) || arity != 2) {
        return enif_make_badarg(env);
    }

    int width, height;
    if (!enif_get_int(env, wh[0], &width) || !enif_get_int(env, wh[1], &height)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowSize(*window, width, height);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_set_window_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_size, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_size_limits(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int minwidth, minheight, maxwidth, maxheight;
    if (!enif_get_int(env, argv[1], &minwidth) ||
        !enif_get_int(env, argv[2], &minheight) ||
        !enif_get_int(env, argv[3], &maxwidth) ||
        !enif_get_int(env, argv[4], &maxheight)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowSizeLimits(*window, minwidth, minheight, maxwidth, maxheight);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_set_window_size_limits(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_size_limits, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_aspect_ratio(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int numer, denom;
    if (!enif_get_int(env, argv[1], &numer) || !enif_get_int(env, argv[2], &denom)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowAspectRatio(*window, numer, denom);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_set_window_aspect_ratio(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_aspect_ratio, env, argc, argv);
}

static ERL_NIF_TERM glfw_window_frame_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    int left, top, right, bottom;
    glfwGetWindowFrameSize(*window, &left, &top, &right, &bottom);

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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    float x_scale, y_scale;
    glfwGetWindowContentScale(*window, &x_scale, &y_scale);

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
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    float opacity = glfwGetWindowOpacity(*window);
    return enif_make_double(env, opacity);
}

static ERL_NIF_TERM nif_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_window_opacity, env, argc, argv);
}

static ERL_NIF_TERM glfw_set_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    double opacity;
    if (!enif_get_double(env, argv[1], &opacity)) {
        return enif_make_badarg(env);
    }

    glfwSetWindowOpacity(*window, opacity);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_set_window_opacity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_set_window_opacity, env, argc, argv);
}

static ERL_NIF_TERM glfw_iconify_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwIconifyWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_iconify_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_iconify_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_restore_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwRestoreWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_restore_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_restore_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_maximize_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwMaximizeWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_maximize_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_maximize_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_show_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwShowWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_show_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_show_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_hide_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwHideWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_hide_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_hide_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_focus_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwFocusWindow(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_focus_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_focus_window, env, argc, argv);
}

static ERL_NIF_TERM glfw_request_window_attention(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GLFWwindow** window;
    if (!enif_get_resource(env, argv[0], glfw_window_resource_type, (void**) &window)) {
        return enif_make_badarg(env);
    }

    glfwRequestWindowAttention(*window);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_request_window_attention(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_request_window_attention, env, argc, argv);
}

static ERL_NIF_TERM glfw_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwPollEvents();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_poll_events(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_poll_events, env, argc, argv);
}

static ERL_NIF_TERM glfw_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glfwPostEmptyEvent();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_post_empty_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return execute_command(glfw_post_empty_event, env, argc, argv);
}


static ErlNifFunc nif_functions[] = {
    {"init_hint", 2, nif_init_hint},
    {"init", 0, nif_init_},
    {"terminate", 0, nif_terminate},
    {"version", 0, nif_version},
    {"version_string", 0, nif_version_string},
    {"last_error", 0, nif_last_error},
    {"error_handler", 0, nif_error_handler},
    {"set_error_handler", 1, nif_set_error_handler},
    {"platform", 0, nif_platform},
    {"platform_supported", 1, nif_platform_supported},

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

    {"poll_events", 0, nif_poll_events},
    {"post_empty_event", 0, nif_post_empty_event}
};

ERL_NIF_INIT(
    glfw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
