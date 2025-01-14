%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%
-module(glfw).
-export([foobar/0]).
-nifs([foobar/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/glfw", 0).

foobar() ->
    erlang:nif_error(nif_library_not_loaded).
