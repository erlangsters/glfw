-module(glfw_test).
-include_lib("eunit/include/eunit.hrl").

glfw_test() ->
    42 = glfw:foobar(),

    ok.
