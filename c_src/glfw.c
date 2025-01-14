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

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    glfw_monitor_resource_type = enif_open_resource_type(env, NULL, "glfw_monitor", glfw_monitor_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (glfw_monitor_resource_type == NULL) {
        fprintf(stderr, "failed to open 'GLFW monitor' resource type\n");
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

static ERL_NIF_TERM nif_init_(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_terminate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_version_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
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

static ERL_NIF_TERM nif_monitors(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_primary_monitor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_work_area(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_physical_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_content_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_set_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_video_modes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_video_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_set_gamma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ERL_NIF_TERM nif_monitor_set_gamma_ramp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
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
    {"monitor_set_gamma_ramp", 2, nif_monitor_set_gamma_ramp}
};

ERL_NIF_INIT(
    glfw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
