%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, January 2025
%%

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
