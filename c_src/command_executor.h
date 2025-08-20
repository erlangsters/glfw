//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#pragma once

#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <erl_nif.h>

typedef struct {
    ErlNifTid thread;
    ErlNifMutex* mutex;
    ErlNifCond* command_ready;
    ErlNifCond* command_done;

    ERL_NIF_TERM (*command_function)(ErlNifEnv*, int, const ERL_NIF_TERM[]);
    ErlNifEnv* command_env;
    int command_argc;
    ERL_NIF_TERM** command_argv;
    ERL_NIF_TERM command_result;

    int command_finished;
    bool should_terminate;
} CommandExecutor;

bool command_executor_init(CommandExecutor* executor);
bool command_executor_destroy(CommandExecutor* executor);

void command_executor_execute(
    CommandExecutor* executor,
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[],
    ERL_NIF_TERM* result
);
