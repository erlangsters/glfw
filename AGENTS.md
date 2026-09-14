# glfw

- `glfw` is a native binding in the `graphics-stack` family.
- Treat it as a near-release binding. Prefer focused edits that preserve API mapping, native resource safety, and platform-specific build behavior.
- Preserve the central positioning of the repository: hand-written GLFW 3.4 binding for the BEAM designed to work with `egl-1.5`.
- For Linux builds and CI, keep the native dependency story explicit: the repository expects system packages such as `cmake`, `libegl-dev`, `libglfw3-dev`, and `libwayland-dev`.
- Do not assume GLFW itself is vendored. Workflow and setup changes should preserve the current Linux system-package model unless the repository intentionally changes its build strategy.
- Keep the BEAM-facing event model centered on handlers and message passing rather than C-style callback functions.
- Preserve the current scope boundaries around contextless windows and the deliberate omission of low-value or off-direction GLFW surface area.
- Keep platform-specific workflow and build requirements explicit instead of forcing them into the pure Erlang library template.
- Preserve the documented macOS caveat. Treat non-Linux platform support as something to verify explicitly before widening claims or automation.
- Keep examples and API shaping Erlang-first.
- Public mapping lives in `docs/api-mapping.md`. Internal status and rationale live in the Binding Surface below. Do not dump the function table into these always-on rules.
- Keep `XXX` comments in source, tests, and public docs until the owning slice replaces them with a written decision here.
- Do not run gamma setters without an explicit go-ahead. They can mutate the OS gamma ramp. Default `glfw_monitor_test` is read-only.

## Binding Surface

This is the internal scope note for what the GLFW binding exposes today, what
remains to land before first release, and what is intentionally outside the
public surface.

The public-facing mapping document lives in `docs/api-mapping.md`. This section
is the authoritative internal inventory. The completion sequence lives in the
family-workspace `GLFW-PLAN.md`.

## Status Vocabulary

- `implemented`: present in the repository and part of the current surface.
- `planned`: intended for the public surface but not landed yet, or present
  but unfinished in a way that first release still owes.
- `deferred`: reasonable later, but not part of the first release target.
- `not planned`: intentionally out of scope unless a concrete use case changes
  the decision.

## Scope Rules

- Keep a single public module: `glfw`.
- Keep windows contextless. OpenGL context and surface creation belong to
  `egl-1.5`. `window_egl_handle/1` is the only native-handle bridge.
- Keep callbacks as handlers: a process receives records such as
  `#glfw_key{}`. Do not return the previously set handler from setters; use
  the matching getter.
- Map GLFW enums to atoms, `GLFW_TRUE`/`GLFW_FALSE` to `boolean()`, and
  success or failure to `{ok, Result}`, `error`, or a specific failure atom
  when the C API has only one failure reason.
- Do not expose user pointers, allocators, proc-address loading, Vulkan, or
  native accessors as public functions.
- Force `GLFW_CLIENT_API = GLFW_NO_API` at window creation. That is the
  contextless invariant, not a temporary hint workaround. Do not add
  `client_api` or context-version window hints. String window hints are UTF-8.
- The binding contract is GLFW 3.4. Compiling against 3.3 is best-effort and
  not a first-release requirement.
- When adding or changing NIF-backed functionality, update the Erlang API,
  the NIF, the relevant test or demo, and the docs together.

## Family Inventory

| Family | Status | Notes |
| --- | --- | --- |
| Initialization | implemented | `init`, `terminate`, `init_hint`, `version`, `version_string`, `get_error`, error handlers, `platform/0`, `platform_supported/1`. |
| Window | implemented | Creation, hints, geometry, state, attributes, title, icon, handlers including `framebuffer_size`, `poll_events`, `post_empty_event`. |
| Monitor | implemented | Query APIs and monitor handler exist. Handles intern by native pointer for the life of `init`. Gamma is implemented and dangerous; default tests only read. |
| Input | implemented | Modes, keys, mouse, cursor objects, input handlers. |
| Joystick / gamepad | implemented | Presence, axes, buttons, hats as `joystick_hat()` atoms, name, GUID, gamepad name/state, mappings, joystick handler. |
| Clipboard | implemented | UTF-8 strings allocated from the Erlang term. |
| EGL window handle | implemented | Follows `glfwGetPlatform()`. Wayland builds a `wl_egl_window` from `glfwGetWaylandWindow` and resizes it from the framebuffer-size callback. X11, Win32, and Cocoa use the platform window handle. |
| Documentation | implemented | Public mapping, extras, README, and missing `-doc` are filled. A later pass owns completeness and consistency of already-written GLFW-paste annotations. |
| Demos | implemented | Event, window, monitor (read-only), input, and joystick demos. Gamma writes are not in any default demo or eunit path. |

## Planned For First Release

These are owed before calling the binding finished. They are not a single
patch.

| Item | Slice | Rationale |
| --- | --- | --- |
| Mods as atom lists | 7 | `#glfw_key{}.mods` and friends are integers. Graphics-stack bitfields are lists of atoms. Align, but not as a drive-by in an unrelated slice. |

## Deferred

Reasonable later, not required to call the first release done.

| Item | Rationale |
| --- | --- |
| Timer functions (`glfwGetTime`, `glfwSetTime`, `glfwGetTimerValue`, `glfwGetTimerFrequency`) | The BEAM already has monotonic time. GLFW timers are convenience for C frame loops, not a missing windowing primitive. |
| GLFW 3.3 compatibility | The contract is 3.4. This machine has 3.4.0. Do not gate first release on Debian 12's 3.3 packages. |
| `get_error/0` rename to `error/0` | Mapped exception is already recorded. Do not rename a settled function for symmetry with `Get`-stripping. |
| `key/2` rename to `window_key/2` | Same. The window argument already disambiguates. Splitting printable vs modifier keys in the type is optional polish, not a blocker. |
| `key_name/1` printable-key helper | Keep the current function. Do not add a predicate unless a demo shows it is needed. |
| Init-hint `default` values | GLFW does not reset hints to a token we should invent. Invalid hint/value pairs are already rejected in Erlang. |
| Extra standard cursors that fail on some platforms | `resize_nwse`, `resize_nesw`, `resize_all`, and `not_allowed` are in the type. Tests comment them out because some platforms emit `GLFW_CURSOR_UNAVAILABLE`. That is upstream behavior, not a missing binding. |
| Child-window / `share` argument on `create_window/3` | See not planned. Deferred only if a later EGL sharing story needs a GLFW-side hook, which is unlikely. |

## Not Planned

Intentionally outside the first public surface.

| Upstream | Rationale |
| --- | --- |
| Child windows / `glfwCreateWindow` share parameter | The share argument is OpenGL context sharing. This binding does not create GL contexts. Sharing belongs to `egl:create_context/4`. Fullscreen-at-create is already covered by `set_window_monitor/7`. |
| `glfwSwapInterval` | Use `egl:swap_interval/2`. |
| `glfwSwapBuffers` as a dirty NIF | Use `egl:swap_buffers/2`. |
| `glfwMakeContextCurrent`, `glfwGetCurrentContext`, `glfwExtensionSupported`, `glfwGetProcAddress` | Contextless windows. Use EGL and the OpenGL bindings. |
| `glfwWaitEvents`, `glfwWaitEventsTimeout` | They block the executor thread and fight the handler model. `poll_events/0` plus a BEAM receive is the replacement. |
| `glfwSetWindowUserPointer`, `glfwGetWindowUserPointer`, monitor and joystick user pointers | Not applicable. Processes and Erlang terms already carry user data. |
| `glfwInitAllocator`, `glfwInitVulkanLoader` | Allocator and loader hooks are C concerns. |
| `GLFW_X11_XCB_VULKAN_SURFACE` init hint | Vulkan is out of scope. |
| All Vulkan functions | `glfwVulkanSupported`, instance extensions, proc address, presentation support, `glfwCreateWindowSurface`. |
| Native accessors as public functions | `glfwGetX11Window`, `glfwGetWaylandWindow`, and the other `glfw3native` symbols stay internal to `window_egl_handle/1`. Do not publish them unless a later platform binding has a concrete need. |
| `glfwGetEGLDisplay`, `glfwGetEGLContext`, `glfwGetEGLSurface` | Would reintroduce GLFW-owned contexts. The EGL binding owns those objects. |
| OSMesa accessors | Off the graphics-stack path. |
| GLFW constants in `glfw.hrl` | Atoms and records are the BEAM-facing surface. Integer tokens stay inside `glfw.erl`. |

## Known Defects

Not design questions. Fix them in the slice that owns the family.

- Wayland `window_egl_handle/1` returns a `wl_egl_window`. EGL
  `create_window_surface/4` then fails with `bad_alloc` when the display
  came from `eglGetDisplay(EGL_DEFAULT_DISPLAY)`. That is an `egl-1.5`
  `eglGetPlatformDisplay` follow-up.
- `#glfw_drop{}.paths` is `[string()]`; confirm whether UTF-8 binaries are
  the better shape before freeze.
- Long `create_window/3` `-doc` prose still describes C Monitor/Share and
  context sharing. Metadata matches `/3`. The later docs pass owns the body.

## Owner Review (slice 2)

The Wayland/X11 handle path, public `framebuffer_size/1`, and restored
`platform/0` / `platform_supported/1` were landed so this machine can move
again. They are not frozen.

Jonathan needs to personally review:

- How Linux native support is implemented: CMake symbol probes,
  `glfwGetPlatform()`, `wl_egl_window` lifetime, and keeping both X11 and
  Wayland backends in one NIF.
- Whether `framebuffer_size/1` and its handler should stay public, stay
  internal to the Wayland EGL handle, or be omitted.
- Whether `platform/0` and `platform_supported/1` belong on the public
  surface or should remain internal to the handle path.

Do not treat those three choices as settled until that review happens.

## Open Questions Owned By Later Slices

These are the surviving questions, recorded so they are not lost.
Source `XXX` comments remain until the owning slice lands.

- Whether `egl-1.5` must grow `eglGetPlatformDisplay` after the Wayland
  handle is correct (only if display creation then fails).
- Gamma ramp implementation review, including Wayland's privileged-protocol
  failure mode. Writes stay out of default eunit and demos.
- `update_gamepad_mappings/1` verification (joystick demo).
- Completeness and consistency of already-written GLFW-paste `-doc` bodies.
