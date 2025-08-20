//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include "command_executor.h"

static void* command_executor_function(void* arg) {
    CommandExecutor* executor = (CommandExecutor*)arg;

    while (true) {
        enif_mutex_lock(executor->mutex);

        /* Wait as long as there is no command AND we are not terminating. */
        while (!executor->should_terminate && executor->command_function == NULL) {
            enif_cond_wait(executor->command_ready, executor->mutex);
        }

        /* If we've been told to stop, break out of the loop. */
        if (executor->should_terminate) {
            enif_mutex_unlock(executor->mutex);
            break;
        }

        /* Execute command */
        executor->command_result = executor->command_function(
            executor->command_env,
            executor->command_argc,
            (const ERL_NIF_TERM**)executor->command_argv
        );

        executor->command_function = NULL;
        executor->command_finished = 1;
        enif_cond_signal(executor->command_done);
        enif_mutex_unlock(executor->mutex);
    }

    return NULL;
}

bool command_executor_init(CommandExecutor* executor) {
    executor->thread = 0;
    executor->mutex = enif_mutex_create("command_executor_mutex");
    executor->command_ready = enif_cond_create("command_executor_ready");
    executor->command_done = enif_cond_create("command_executor_done");

    if (!executor->mutex || !executor->command_ready || !executor->command_done) {
        if (executor->mutex) enif_mutex_destroy(executor->mutex);
        if (executor->command_ready) enif_cond_destroy(executor->command_ready);
        if (executor->command_done) enif_cond_destroy(executor->command_done);
        return false;
    }

    executor->command_function = NULL;
    executor->command_env = NULL;
    executor->command_argc = 0;
    executor->command_argv = NULL;

    executor->command_finished = 0;
    executor->should_terminate = false;

    if (enif_thread_create("command_executor_thread", &executor->thread, command_executor_function, executor, NULL) != 0) {
        enif_mutex_destroy(executor->mutex);
        enif_cond_destroy(executor->command_ready);
        enif_cond_destroy(executor->command_done);
        return false;
    }
    return true;
}

bool command_executor_destroy(CommandExecutor* executor) {
    enif_mutex_lock(executor->mutex);
    executor->should_terminate = true;
    enif_cond_signal(executor->command_ready);
    enif_mutex_unlock(executor->mutex);

    void* result;
    enif_thread_join(executor->thread, &result);
    
    enif_mutex_destroy(executor->mutex);
    enif_cond_destroy(executor->command_ready);
    enif_cond_destroy(executor->command_done);
    
    return true;
}

void command_executor_execute(
    CommandExecutor* executor,
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[],
    ERL_NIF_TERM* result
) {
    enif_mutex_lock(executor->mutex);

    executor->command_function = function;
    executor->command_env = env;
    executor->command_argc = argc;
    executor->command_argv = argv;
    executor->command_finished = 0;

    enif_cond_signal(executor->command_ready);

    while (!executor->command_finished) {
        enif_cond_wait(executor->command_done, executor->mutex);
    }

    *result = executor->command_result;
    enif_mutex_unlock(executor->mutex);
}
