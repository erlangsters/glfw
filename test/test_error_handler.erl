-module(test_error_handler).
-export([setup/0]).

-include_lib("glfw/include/glfw.hrl").

setup() ->
    glfw:set_error_handler(spawn(fun handler/0)).

handler() ->
    receive
        #glfw_error{code = Code, description = Description} ->
            io:format("GLFW Error: ~p - ~s~n", [Code, Description]),
            handler()
    end.
