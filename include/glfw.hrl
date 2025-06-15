%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%

-record(glfw_error, {
    code :: glfw:error_code(),
    description :: glfw:error_description()
}).

-record(glfw_video_mode, {
    width :: integer(),
    height :: integer(),
    red_bits :: integer(),
    green_bits :: integer(),
    blue_bits :: integer(),
    refresh_rate :: integer()
}).

-record(glfw_gamma_ramp, {
    red :: [integer()],
    green :: [integer()],
    blue :: [integer()]
}).

-record(glfw_image, {
    width :: integer(),
    height :: integer(),
    pixels :: binary()
}).

-record(glfw_window_position, {
    window :: glfw:window(),
    position :: {X :: integer(), Y :: integer()}
}).

-record(glfw_window_size, {
    window :: glfw:window(),
    size :: {Width :: integer(), Height :: integer()}
}).

-record(glfw_window_close, {
    window :: glfw:window()
}).

-record(glfw_window_refresh, {
    window :: glfw:window()
}).

-record(glfw_window_focus, {
    window :: glfw:window(),
    focused :: boolean()
}).

-record(glfw_window_iconify, {
    window :: glfw:window(),
    iconified :: boolean()
}).

-record(glfw_window_maximize, {
    window :: glfw:window(),
    maximized :: boolean()
}).

-record(glfw_window_content_scale, {
    window :: glfw:window(),
    scale :: {X :: float(), Y :: float()}
}).

-record(glfw_key, {
    window :: glfw:window(),
    key :: glfw:key(),
    scancode :: glfw:scancode(),
    action :: press | release | repeat,
    mods :: integer()
}).

-record(glfw_char, {
    window :: glfw:window(),
    codepoint :: erlang:char()
}).

-record(glfw_char_mods, {
    window :: glfw:window(),
    codepoint :: erlang:char(),
    mods :: integer()
}).

-record(glfw_mouse_button, {
    window :: glfw:window(),
    button :: glfw:mouse_button(),
    action :: press | release,
    mods :: integer()
}).

-record(glfw_cursor_position, {
    window :: glfw:window(),
    position :: {X :: float(), Y :: float()}
}).

-record(glfw_cursor_enter, {
    window :: glfw:window(),
    entered :: boolean()
}).
