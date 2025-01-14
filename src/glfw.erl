-module(glfw).
-export([foobar/0]).
-nifs([foobar/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./priv/glfw", 0).

foobar() ->
    erlang:nif_error(nif_library_not_loaded).
