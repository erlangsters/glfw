# glfw

- `glfw` is a native binding in the `graphics-stack` family.
- Treat it as a near-release binding. Prefer focused edits that preserve API mapping, native resource safety, and platform-specific build behavior.
- Preserve the central positioning of the repository: hand-written GLFW 3.4 binding for the BEAM designed to work with `egl-1.5`.
- For Linux builds and CI, keep the native dependency story explicit: the repository expects system packages such as `cmake`, `libegl-dev`, and `libglfw3-dev`.
- Do not assume GLFW itself is vendored. Workflow and setup changes should preserve the current Linux system-package model unless the repository intentionally changes its build strategy.
- Keep the BEAM-facing event model centered on handlers and message passing rather than C-style callback functions.
- Preserve the current scope boundaries around contextless windows and the deliberate omission of low-value or off-direction GLFW surface area.
- Keep platform-specific workflow and build requirements explicit instead of forcing them into the pure Erlang library template.
- Preserve the documented macOS caveat. Treat non-Linux platform support as something to verify explicitly before widening claims or automation.
- Keep examples and API shaping Erlang-first.
- Public mapping lives in `docs/api-mapping.md`. Internal status and rationale live in the Binding Surface below. Do not dump the function table into these always-on rules.
- Keep `XXX` comments in source, tests, and public docs until the owning slice replaces them with a written decision here.
- Do not run `glfw_monitor_test` or gamma setters without an explicit go-ahead. They can mutate the OS gamma ramp.

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
  `client_api` or context-version window hints.
- The binding contract is GLFW 3.4. Compiling against 3.3 is best-effort and
  not a first-release requirement.
- When adding or changing NIF-backed functionality, update the Erlang API,
  the NIF, the relevant test or demo, and the docs together.

## Family Inventory

| Family | Status | Notes |
| --- | --- | --- |
| Initialization | implemented | `init`, `terminate`, `init_hint`, `version`, `version_string`, `get_error`, error handlers. `platform/0` and `platform_supported/1` are documented and tested but currently commented out; they are planned (slice 2). |
| Window | implemented | Creation, hints, geometry, state, attributes, handlers, `poll_events`, `post_empty_event`. See planned items below for title getter, icon, framebuffer size, and handler correctness. |
| Monitor | implemented | Query APIs and monitor handler exist. Handle identity is wrong (new resource per call). Gamma is implemented and dangerous. |
| Input | implemented | Modes, keys, mouse, cursor objects, input handlers. Joystick hats are returned as raw integers despite `joystick_hat()`. |
| Joystick / gamepad | implemented | Presence, axes, buttons, name, GUID, gamepad name/state, mappings, joystick handler. Hats unpacking is planned. |
| Clipboard | implemented | Fixed 1024-byte setter buffer. Planned to allocate dynamically. |
| EGL window handle | planned | Works on X11, Win32, and Cocoa. Linux always takes the X11 path, so Wayland sessions fail. |
| Documentation | planned | Mapping table exists; many `-doc` blocks are still `To be written`. Follow `glm` patterns family by family (slice 6). |
| Demos | planned | Event and joystick demos run. Window and monitor demos are stubs. Input demo is empty (slice 3). |

## Planned For First Release

These are owed before calling the binding finished. They are not a single
patch.

| Item | Slice | Rationale |
| --- | --- | --- |
| Wayland `window_egl_handle/1` | 2 | Compile-time `#ifdef linux → X11` is wrong. Follow `glfwGetPlatform()`. Wayland needs a `wl_egl_window` created from `glfwGetWaylandWindow` (`wl_surface*`), then resized and destroyed with the window. |
| `platform/0`, `platform_supported/1` | 2 | Already mapped and tested. Commented out as a GLFW 3.3 workaround. Needed as the runtime switch for the native handle. |
| `framebuffer_size` and its handler | 2 | Marked N/A today. Wayland almost certainly needs framebuffer size to resize `wl_egl_window`. Slice 2 decides whether this is public or an internal signal. Do not leave the N/A decision standing without that check. |
| Interactive demos | 3 | Right assessment tool for a windowing binding. Finish window, monitor (read-only), and input. Keep event and joystick. |
| Quarantine gamma | 3 | `set_gamma/2` and `set_gamma_ramp/2` stay in the API. They must not run from default eunit or a default demo command. |
| Monitor handle identity | 4 | `monitors/0` and `primary_monitor/0` mint a new resource every call. The same `GLFWmonitor*` must intern to the same Erlang term for the life of `init`, including the monitor handler. |
| `terminate` / destroy resource safety | 4 | `glfwTerminate` can free objects that live Erlang terms still point at. Resource destructors are no-ops. Destroy and terminate must poison resources so later calls fail cleanly. |
| `create_window` / `destroy_window` finish | 4 | Input handler fields are not initialized. Destroy does not clear the native pointer. Title is read as Latin-1 on create and UTF-8 on set. |
| Handler pid storage | 3 or 4 | Some setters `enif_make_copy` the pid term, some store `argv[1]` directly, then `enif_send` casts the term to `ErlNifPid*`. Store a real `ErlNifPid` via `enif_get_local_pid`. |
| `window_title/1` | 5 | Getter exists upstream and is mapped; the NIF is commented out. |
| `set_window_icon/2` | 5 | NIF currently returns `42`. Either implement it from `#glfw_image{}` or mark it deferred and stop advertising it. Prefer implement: the type and test already exist. |
| Unpack `joystick_hats/1` | 5 | Public type is already `joystick_hat()`. The NIF returns integers. |
| Clipboard setter buffer | 5 | `char string[1024]` cannot stand. Allocate from the Erlang string length. |
| UTF-8 vs Latin-1 per hint | 5 | String window hints should be UTF-8. Confirm each hint against the GLFW spec rather than guessing. |
| `GLFW_NO_API` invariant | 5 | Keep forcing `GLFW_CLIENT_API = GLFW_NO_API` after user hints. Document it as the contextless rule, not as "window hints are unfinished". |
| `dont_care` on size limits and aspect ratio | 6 | Already implemented. Document the slightly wider interface in `docs/api-mapping.md`. |
| `monitor_set_handler/1` name | 5 | Mapping rule and every other setter use `set_*_handler`. The monitor setter is the odd one out. Rename to `set_monitor_handler/1` before first release. |
| Mods as atom lists | 6 or 7 | `#glfw_key{}.mods` and friends are integers. Graphics-stack bitfields are lists of atoms. Align, but not as a drive-by in an unrelated slice. |

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

- `set_window_icon/2` returns integer `42`. The window test asserts that.
- `create_window` does not initialize input handler fields on
  `GLFWWindowResource`.
- Handler send path casts an `ERL_NIF_TERM` to `ErlNifPid*`.
- Linux `window_egl_handle/1` always calls `glfwGetX11Window`.
- `glfw_test` calls `platform/0` and `platform_supported/1` while those
  exports are commented out.
- `poll_events/0` and `post_empty_event/0` `-doc` `see_also` entries point at
  `wait_events/0` and `wait_events_timeout/1`, which are not implemented.
- `#glfw_drop{}.paths` is `[string()]`; confirm whether UTF-8 binaries are
  the better shape before freeze.
- `create_window/3` docs still describe Monitor and Share parameters the
  function does not take.

## Open Questions Owned By Later Slices

These are the surviving `XXX` questions, recorded so they are not lost.
Source and public-doc `XXX` comments remain until the owning slice lands.

- Pointer lifetime of monitor, window, and cursor resources after
  `terminate/0` and after native destroy (slice 4).
- Whether a monitor from `#glfw_monitor{}` must compare equal to the same
  monitor later returned by `monitors/0` (slice 4: yes, intern by pointer).
- Whether `framebuffer_size` is public or internal once Wayland needs it
  (slice 2).
- Whether `egl-1.5` must grow `eglGetPlatformDisplay` after the Wayland
  handle is correct (slice 2, only if display creation then fails).
- Gamma ramp implementation review, including Wayland's privileged-protocol
  failure mode (slice 3, opt-in only).
- `update_gamepad_mappings/1` verification (slice 3, joystick demo).
- `window_monitor/1` and `set_window_monitor/7` verification (slice 3,
  window demo). `undefined` vs error remains: use `get_error/0`, same as
  `primary_monitor/0`.
