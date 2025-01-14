#include <string.h>
#include <erl_nif.h>

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    return 0;
}

static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)
{
    return 0;
}

static ERL_NIF_TERM nif_foobar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ErlNifFunc nif_functions[] = {
    {"foobar", 0, nif_foobar}
};

ERL_NIF_INIT(
    glfw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
