# API mapping

This is a binding of GLFW 3.4. Existing GLFW knowledge still applies. The mapping is small, consistent, and documented here when it is not obvious.

The public module is `glfw`. Event records live in `glfw.hrl`.

## Mapping rules

- GLFW enums become atoms whenever a closed set exists. For instance, `input_mode()` is `cursor | sticky_keys | sticky_mouse_buttons | lock_key_mods | raw_mouse_motion`.
- An integer that GLFW documents as only `GLFW_TRUE` or `GLFW_FALSE` becomes `boolean()`.
- Success and failure follow the C failure shape. `{ok, Result}` and `error` when GLFW has more than one failure reason that the binding cannot distinguish (for example `gamepad_name/1`). A specific atom when there is only one failure reason (for example `joystick_buttons/1` returns `not_present | [press | release]`).
- GLFW `Pos` becomes `position`. `glfwSetCursorPosCallback` is `set_cursor_position_handler/2`.
- A GLFW `Get` prefix is dropped. `glfwGetCursorPos` / `glfwSetCursorPos` become `cursor_position/1` and `set_cursor_position/2`. Exception: `glfwGetError` stays `get_error/0`.
- C callbacks become handlers. You register a pid; it receives a record such as `#glfw_key{}`. Setters do not return the previously set handler. Use the matching getter. Pass `undefined` to unregister.
- User pointers, allocators, and proc-address loading are not implemented. Processes and Erlang terms already carry user data.
- GLFW integer constants are not exported from `glfw.hrl`. Atoms and records are the BEAM-facing surface.

## Initialization

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwInit` | `init/0` | N/A |
| `glfwTerminate` | `terminate/0` | Poisons live window, cursor, and monitor resources. Later calls on those terms raise `badarg`. |
| `glfwInitHint` | `init_hint/2` | Invalid hint/value pairs are rejected in Erlang before the NIF. There is no invented `default` token. `GLFW_X11_XCB_VULKAN_SURFACE` is not implemented. |
| `glfwInitAllocator` | N/A | C allocator hook. |
| `glfwInitVulkanLoader` | N/A | Vulkan is out of scope. |
| `glfwGetVersion` | `version/0` | N/A |
| `glfwGetVersionString` | `version_string/0` | N/A |
| `glfwGetError` | `get_error/0` | Not renamed to `error/0`. Returns `no_error` or `{error, error_code(), error_description()}`. |
| `glfwSetErrorCallback` | `error_handler/0` and `set_error_handler/1` | The event is `#glfw_error{}`. |
| `glfwGetPlatform` | `platform/0` | N/A |
| `glfwPlatformSupported` | `platform_supported/1` | N/A |

Monitor, window, and cursor resources intern by native pointer for the life of `init/0`. A monitor from `#glfw_monitor{}` compares equal to the handle from `monitors/0` for the same display.

## Context

Windows are [contextless](contextless-windows.md). OpenGL context, current-context, swap, and proc-address operations belong to EGL and the OpenGL bindings. None of the GLFW context functions are implemented.

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwMakeContextCurrent` | N/A | Use `egl:make_current/4`. |
| `glfwGetCurrentContext` | N/A | Use `egl:get_current_context/0`. |
| `glfwSwapInterval` | N/A | Use `egl:swap_interval/2`. |
| `glfwSwapBuffers` | N/A | Use `egl:swap_buffers/2`. |
| `glfwExtensionSupported` | N/A | Use `gl:get_string/1`. The OpenGL bindings do not load extensions. |
| `glfwGetProcAddress` | N/A | N/A |

## Window

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwDefaultWindowHints` | `default_window_hints/0` | N/A |
| `glfwWindowHint` | `window_hint/2` | No `client_api` hint. See `create_window/3`. |
| `glfwWindowHintString` | `window_hint/2` | String hints (`cocoa_frame_name`, `wayland_app_id`, `x11_instance_name`, `x11_class_name`) are UTF-8. |
| `glfwCreateWindow` | `create_window/3` | Width, height, and UTF-8 title. No monitor or share arguments. Always forces `GLFW_CLIENT_API = GLFW_NO_API` after user hints. |
| `glfwDestroyWindow` | `destroy_window/1` | Poisons the window resource. |
| `glfwWindowShouldClose` | `window_should_close/1` | N/A |
| `glfwSetWindowShouldClose` | `set_window_should_close/2` | N/A |
| `glfwGetWindowTitle` | `window_title/1` | UTF-8. `undefined` if GLFW reports none. |
| `glfwSetWindowTitle` | `set_window_title/2` | UTF-8. |
| `glfwSetWindowIcon` | `set_window_icon/2` | `[#glfw_image{}]`. `[]` reverts to the default icon. Wayland and macOS emit `feature_unavailable`; the call still returns `ok`. |
| `glfwGetWindowPos` | `window_position/1` | N/A |
| `glfwSetWindowPos` | `set_window_position/2` | N/A |
| `glfwGetWindowSize` | `window_size/1` | N/A |
| `glfwSetWindowSizeLimits` | `set_window_size_limits/3` | Each limit is `{Width, Height}` or `dont_care`. A component may also be `dont_care`. |
| `glfwSetWindowAspectRatio` | `set_window_aspect_ratio/2` | `{Numerator, Denominator}` or `dont_care`. A component may also be `dont_care`. |
| `glfwSetWindowSize` | `set_window_size/2` | N/A |
| `glfwGetFramebufferSize` | `framebuffer_size/1` | Public. The native callback also stays installed so Wayland can resize the EGL window handle. |
| `glfwGetWindowFrameSize` | `window_frame_size/1` | N/A |
| `glfwGetWindowContentScale` | `window_content_scale/1` | N/A |
| `glfwGetWindowOpacity` | `window_opacity/1` | N/A |
| `glfwSetWindowOpacity` | `set_window_opacity/2` | N/A |
| `glfwIconifyWindow` | `iconify_window/1` | N/A |
| `glfwRestoreWindow` | `restore_window/1` | N/A |
| `glfwMaximizeWindow` | `maximize_window/1` | N/A |
| `glfwShowWindow` | `show_window/1` | N/A |
| `glfwHideWindow` | `hide_window/1` | N/A |
| `glfwFocusWindow` | `focus_window/1` | N/A |
| `glfwRequestWindowAttention` | `request_window_attention/1` | N/A |
| `glfwGetWindowMonitor` | `window_monitor/1` | `undefined` may mean no monitor or an error. Use `get_error/0`. |
| `glfwSetWindowMonitor` | `set_window_monitor/7` | Fullscreen-at-create is not a `create_window/3` argument. Use this after creation. |
| `glfwGetWindowAttrib` | `window_attrib/2` | N/A |
| `glfwSetWindowAttrib` | `set_window_attrib/3` | N/A |
| `glfwSetWindowUserPointer` | N/A | N/A |
| `glfwGetWindowUserPointer` | N/A | N/A |
| `glfwSetWindowPosCallback` | `window_position_handler/1` and `set_window_position_handler/2` | `#glfw_window_position{}`. |
| `glfwSetWindowSizeCallback` | `window_size_handler/1` and `set_window_size_handler/2` | `#glfw_window_size{}`. |
| `glfwSetWindowCloseCallback` | `window_close_handler/1` and `set_window_close_handler/2` | `#glfw_window_close{}`. |
| `glfwSetWindowRefreshCallback` | `window_refresh_handler/1` and `set_window_refresh_handler/2` | `#glfw_window_refresh{}`. |
| `glfwSetWindowFocusCallback` | `window_focus_handler/1` and `set_window_focus_handler/2` | `#glfw_window_focus{}`. |
| `glfwSetWindowIconifyCallback` | `window_iconify_handler/1` and `set_window_iconify_handler/2` | `#glfw_window_iconify{}`. |
| `glfwSetWindowMaximizeCallback` | `window_maximize_handler/1` and `set_window_maximize_handler/2` | `#glfw_window_maximize{}`. |
| `glfwSetFramebufferSizeCallback` | `framebuffer_size_handler/1` and `set_framebuffer_size_handler/2` | `#glfw_framebuffer_size{}`. The native callback stays installed for Wayland EGL resize even when no handler is registered. |
| `glfwSetWindowContentScaleCallback` | `window_content_scale_handler/1` and `set_window_content_scale_handler/2` | `#glfw_window_content_scale{}`. |
| `glfwPollEvents` | `poll_events/0` | Sends handler messages. See [thread safety](thread-safety.md). |
| `glfwWaitEvents` | N/A | Would block the executor thread. Use `poll_events/0` and a BEAM `receive`. |
| `glfwWaitEventsTimeout` | N/A | Same. |
| `glfwPostEmptyEvent` | `post_empty_event/0` | N/A |

## Monitor

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwGetMonitors` | `monitors/0` | `[]` may mean no monitors or an error. Use `get_error/0`. Handles intern by native pointer. |
| `glfwGetPrimaryMonitor` | `primary_monitor/0` | `undefined` may mean no monitor or an error. The same native monitor is the same Erlang term as in `monitors/0`. |
| `glfwGetMonitorPos` | `monitor_position/1` | N/A |
| `glfwGetMonitorWorkarea` | `monitor_work_area/1` | N/A |
| `glfwGetMonitorPhysicalSize` | `monitor_physical_size/1` | N/A |
| `glfwGetMonitorContentScale` | `monitor_content_scale/1` | N/A |
| `glfwGetMonitorName` | `monitor_name/1` | N/A |
| `glfwSetMonitorUserPointer` | N/A | N/A |
| `glfwGetMonitorUserPointer` | N/A | N/A |
| `glfwSetMonitorCallback` | `monitor_handler/0` and `set_monitor_handler/1` | `#glfw_monitor{}`. |
| `glfwGetVideoModes` | `video_modes/1` | N/A |
| `glfwGetVideoMode` | `video_mode/1` | N/A |
| `glfwSetGamma` | `set_gamma/2` | Implemented and dangerous. It can change OS display calibration. Not used by default tests or demos. |
| `glfwGetGammaRamp` | `gamma_ramp/1` | N/A |
| `glfwSetGammaRamp` | `set_gamma_ramp/2` | Same warning as `set_gamma/2`. On Wayland, gamma is often a privileged protocol and may emit `feature_unavailable`. |

## Input

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwGetInputMode` | `input_mode/2` | N/A |
| `glfwSetInputMode` | `set_input_mode/3` | N/A |
| `glfwRawMouseMotionSupported` | `raw_mouse_motion_supported/0` | N/A |
| `glfwGetKeyName` | `key_name/1` | `{key, key()} \| {scancode, scancode()}`. |
| `glfwGetKeyScancode` | `key_scancode/1` | N/A |
| `glfwGetKey` | `key/2` | Not renamed to `window_key/2`. |
| `glfwGetMouseButton` | `mouse_button/2` | N/A |
| `glfwGetCursorPos` | `cursor_position/1` | N/A |
| `glfwSetCursorPos` | `set_cursor_position/2` | N/A |
| `glfwCreateCursor` | `create_cursor/2` | `#glfw_image{}` and a hotspot `{X, Y}`. Pixels are an RGBA binary. |
| `glfwCreateStandardCursor` | `create_standard_cursor/1` | Some shapes emit `cursor_unavailable` on some platforms. |
| `glfwDestroyCursor` | `destroy_cursor/1` | Poisons the cursor resource. |
| `glfwSetCursor` | `set_cursor/2` | `default` restores the regular cursor. |
| `glfwSetKeyCallback` | `key_handler/1` and `set_key_handler/2` | `#glfw_key{}`. `mods` is an integer. |
| `glfwSetCharCallback` | `char_handler/1` and `set_char_handler/2` | `#glfw_char{}`. |
| `glfwSetCharModsCallback` | `char_mods_handler/1` and `set_char_mods_handler/2` | `#glfw_char_mods{}`. `mods` is an integer. |
| `glfwSetMouseButtonCallback` | `mouse_button_handler/1` and `set_mouse_button_handler/2` | `#glfw_mouse_button{}`. `mods` is an integer. |
| `glfwSetCursorPosCallback` | `cursor_position_handler/1` and `set_cursor_position_handler/2` | `#glfw_cursor_position{}`. |
| `glfwSetCursorEnterCallback` | `cursor_enter_handler/1` and `set_cursor_enter_handler/2` | `#glfw_cursor_enter{}`. |
| `glfwSetScrollCallback` | `scroll_handler/1` and `set_scroll_handler/2` | `#glfw_scroll{}`. |
| `glfwSetDropCallback` | `drop_handler/1` and `set_drop_handler/2` | `#glfw_drop{}`. `paths` is `[string()]`. |

Modifier bits in `#glfw_key{}`, `#glfw_char_mods{}`, and `#glfw_mouse_button{}` are integers. Lists of atoms are a later mapping change, not part of this surface.

## Joystick and gamepad

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwJoystickPresent` | `joystick_present/1` | N/A |
| `glfwGetJoystickAxes` | `joystick_axes/1` | `not_present \| [float()]`. |
| `glfwGetJoystickButtons` | `joystick_buttons/1` | `not_present \| [press \| release]`. |
| `glfwGetJoystickHats` | `joystick_hats/1` | `not_present \| [joystick_hat()]`. Atoms such as `hat_up` and `hat_right_up`, not integers or bit lists. |
| `glfwGetJoystickName` | `joystick_name/1` | N/A |
| `glfwGetJoystickGUID` | `joystick_guid/1` | N/A |
| `glfwSetJoystickUserPointer` | N/A | N/A |
| `glfwGetJoystickUserPointer` | N/A | N/A |
| `glfwJoystickIsGamepad` | `joystick_is_gamepad/1` | N/A |
| `glfwSetJoystickCallback` | `joystick_handler/0` and `set_joystick_handler/1` | `#glfw_joystick{}`. |
| `glfwUpdateGamepadMappings` | `update_gamepad_mappings/1` | N/A |
| `glfwGetGamepadName` | `gamepad_name/1` | `{ok, string()} \| error`. |
| `glfwGetGamepadState` | `gamepad_state/1` | N/A |

## Clipboard and time

| GLFW | Binding | Notes |
| --- | --- | --- |
| `glfwGetTime` | N/A | The BEAM already has monotonic time. |
| `glfwSetTime` | N/A | Same. |
| `glfwGetTimerValue` | N/A | Same. |
| `glfwGetTimerFrequency` | N/A | Same. |
| `glfwSetClipboardString` | `set_clipboard_string/2` | UTF-8. The string is allocated from the Erlang term. The window may be `undefined`. |
| `glfwGetClipboardString` | `clipboard_string/1` | The window may be `undefined`. |

## EGL window handle

| GLFW | Binding | Notes |
| --- | --- | --- |
| *(none)* | `window_egl_handle/1` | The only public native-handle bridge. See [contextless windows](contextless-windows.md). |

## Vulkan

Vulkan is out of scope. These functions are not implemented:

- `glfwVulkanSupported`
- `glfwGetRequiredInstanceExtensions`
- `glfwGetInstanceProcAddress`
- `glfwGetPhysicalDevicePresentationSupport`
- `glfwCreateWindowSurface`

The `GLFW_X11_XCB_VULKAN_SURFACE` init hint is not implemented.

## Native accessors

Native accessors are not public. `window_egl_handle/1` uses `glfwGetWin32Window`, `glfwGetCocoaWindow`, `glfwGetX11Window`, and `glfwGetWaylandWindow` internally, and on Wayland creates a `wl_egl_window` owned by the window resource.

`glfwGetEGLDisplay`, `glfwGetEGLContext`, `glfwGetEGLSurface`, OSMesa accessors, and the other `glfw3native` symbols stay unpublished. A later platform binding can ask for a specific accessor if it has a concrete need.
