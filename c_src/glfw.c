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
#include <erl_nif.h>
#include <GLFW/glfw3.h>

static ErlNifResourceType* glfw_monitor_resource_type = NULL;

static void glfw_monitor_resource_dtor(ErlNifEnv* env, void* obj) {
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    glfw_monitor_resource_type = enif_open_resource_type(env, NULL, "glfw_monitor", glfw_monitor_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (glfw_monitor_resource_type == NULL) {
        fprintf(stderr, "failed to open 'GLFW monitor' resource type\n");
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
