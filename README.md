# GLFW binding for the BEAM

A binding of GFLW for the BEAM known to work with the
[EGL 1.5](https://github.com/erlangsters/egl-1.5) binding.

:construction: It's a work-in-progress, use at your own risk.

Written by the Erlangsters [community](https://www.erlangsters.org/) and
released under the MIT [license](/https://opensource.org/license/mit).

## Getting started

XXX: To be written.

## Using it in your project

With the **Rebar3** build system, add the following to the `rebar.config` file
of your project.

```
{deps, [
  {glfw, {git, "https://github.com/erlangsters/glfw.git", {tag, "master"}}}
]}.
```

If you happen to use the **Erlang.mk** build system, then add the following to
your Makefile.

```
BUILD_DEPS = glfw
dep_glfw = git https://github.com/erlangsters/glfw master
```

In practice, you want to replace the branch "master" with a specific "tag" to
avoid breaking your project if incompatible changes are made.
