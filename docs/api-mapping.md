# API Mapping

It's binding of an existing library and therefore knowledge of GLFW itself is
expected.

XXX: Talk about pointer lifetime.
 100% of your existing GLFW knowledge is
transferable with a few notes.

XXX: Implement child windows.

XXX: Verify/clarify  how glfwTerminate() could corrupt the BEAM by invalidating
     pointer to exisitng resource (cursor, window, etc).

XXX: What happens if monitor resource sent in the monitor callback, then later
     another retrieve with monitors/0. Should they be shared ?

XXX: Verify it builds on earlier version of GLFW (version 3.3 on debian 12 for instance).

XXX: Should time-related functions be implemented

- glfwGetTimerFrequency
- glfwGetTimerValue
- glfwGetTime
- glfwSetTime


## Idiomatic API

This binding follows a very consistent set of mapping rules.
All those rules are documented (unless trivial) and when exceptions are made, they're documented
as well.
You do not need to really formally understand those rules as most of the times,
this is to make the API more idiomatic to use in Erlang and Elixir
Equipped with common sense and the API reference, you will not fight th ebinding.

If there's still ambiguity, contact us.

## Mapping Rules

- GLFW enums are implemented as atoms. Whenver possible. For instance:

```erlang
-type input_mode() ::
    cursor |
    sticky_keys |
    sticky_mouse_buttons |
    lock_key_mods |
    raw_mouse_motion
.
```

- When function takes or returns an integer which is documented to be only
`GLFW_TRUE` or `GLFW_FALSE`, it becomes a `boolean()`.

- Use of `{ok, Result}` and `error` when applicable, to denotate success/failure.
When the error as a value, it becomes `{error, Error}`.
In getGamepadName() because there can be more than one reason for the error, and we cannot determine it (joysitkc not present, does not have a mapping)

- When the error is specific, it uses different. For instance:
`not_present | [float()].`
In getJoystickButtons() because it can only fail if joystick if not present (returns a false)

- All Set/Get XXX UserPointer is not implemented as not application in Erlang. Indeed, it's easy to apss data around.

- GLFW likes `Pos` shortcut. In the binding it's `position`.
For instance: `glfwSetCursorPosCallback` becomes `set_cursor_position_handler`

- GFLW prefix with Get, this binding removes it.
For instance: glfwSet/GetCursorPos becomes `cursor_position` and `set_cursor_position`
Exception: `glfwGetError` remains `get_error` in binding.

- Regarding callback functions. They're implemented as handlers. It uses a record to distinguish the event.
Note that unlike C, The previously set callback are not returend (or handlers) unlike C api
Instead there's a getter. For instnace: `key_handler()` to retrive handler.

- Allocator and ProcAddress stuff not implemented
For instance: glfwInitAllocator and glfwGetProcAddress


More NOTES:

- GLFW constants are not present in the glfw.hrl header

## Functions Tables

Here is the list of all GLFW functions, organized into categories, with their
equivalent. If exceptions where to be made, it's in the notes.

**Initialization**

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwInit` | `init` | N/A |
| `glfwTerminate` | `terminate` | XXX: invalid all existing objects ?  |
| `glfwInitHint` | `init_hint` | Passing an invalid combination of hint/value will be caught before it reaches the NIF function. Also, the `GLFW_X11_XCB_VULKAN_SURFACE` hint in not implemented. |
| `glfwInitAllocator` | N/A | N/A |
| `glfwInitVulkanLoader` | N/A | N/A |
| `glfwGetVersion` | `version` | N/A |
| `glfwGetVersionString` | `version_string` | N/A |
| `glfwGetError` | `get_error` | XXX: Rename to `error/0` for consistency? |
| `glfwSetErrorCallback` | `error_handler` and `set_error_handler` | The event is `#glfw_error{}`. |
| `glfwGetPlatform` | `platform` | N/A |
| `glfwPlatformSupported` | `platform_supported` | N/A |

**Context**

The only applicable function is `glfwSwapInterval` and therefore is the only
one that is implemented. The other functions are about operating on the
underlying OpenGL contexts which we do not do with this binding (windows are
[contextless](docs/contextless-windows.md)).

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwMakeContextCurrent` | N/A | Use `egl:swap_buffers/2` instead. |
| `glfwGetCurrentContext` | N/A | Use `egl:get_current_context/0` instead. |
| `glfwSwapInterval` | `swap_interval` | XXX: To be implemented? Not sure. |
| `glfwExtensionSupported` | N/A | Use `gl:get_string/1` instead. Note that the OpenGL bindings do not support loading extensions. |
| `glfwGetProcAddress` | N/A | N/A |

**Window**

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwDefaultWindowHints` | `default_window_hints` | N/A |
| `glfwWindowHint` | `window_hint` | XXX: Double-check UTF8/Latin1 conversation (per hint). |
| `glfwWindowHintString` | `window_hint` | Use `window_hint/2` instead. |
| `glfwCreateWindow` | `create_window` | XXX: Implementation to be finalized. |
| `glfwDestroyWindow` | `destroy_window` | XXX: Implementation to be finalized. |
| `glfwWindowShouldClose` | `window_should_close` | N/A |
| `glfwSetWindowShouldClose` | `set_window_should_close` | N/A |

| `glfwGetWindowTitle` | `window_title` | N/A |
| `glfwSetWindowTitle` | `set_window_title` | N/A |

| `glfwSetWindowIcon` | `set_window_icon`      | XXX: To be implement         |

| `glfwGetWindowPos` | `window_position` | N/A |
| `glfwSetWindowPos` | `set_window_position` | N/A |
| `glfwGetWindowSize` | `window_size` | N/A |
| `glfwSetWindowSizeLimits` | `set_window_size_limits` | N/A |
| `glfwSetWindowAspectRatio` | `set_window_aspect_ratio` | N/A |
| `glfwSetWindowSize` | `set_window_size` | N/A |
| `glfwGetFramebufferSize` | N/A | N/A XXX: Document why it's N/A |
| `glfwGetWindowFrameSize` | `window_frame_size` | N/A |
| `glfwGetWindowContentScale` | `window_content_scale` | N/A |

| `glfwGetWindowOpacity` | `window_opacity` | N/A |
| `glfwSetWindowOpacity` | `set_window_opacity` | N/A |
| `glfwIconifyWindow` | `iconify_window` | N/A |
| `glfwRestoreWindow` | `restore_window` | N/A |
| `glfwMaximizeWindow` | `maximize_window` | N/A |
| `glfwShowWindow` | `show_window` | N/A |
| `glfwHideWindow` | `hide_window` | N/A |
| `glfwFocusWindow` | `focus_window` | N/A |
| `glfwRequestWindowAttention` | `request_window_attention` | N/A |
| `glfwGetWindowMonitor` | `window_monitor` | N/A XXX: Verify implementation. |
| `glfwSetWindowMonitor` | `set_window_monitor` | We can't distinguish between no monitor or error. Therefore a `undefined` return value might mean error. XXX: Verify implementation. |
| `glfwGetWindowAttrib` | `window_attrib` | N/A |
| `glfwSetWindowAttrib` | `set_window_attrib` | N/A |
| `glfwSetWindowUserPointer` | N/A | N/A |
| `glfwGetWindowUserPointer` | N/A | N/A |
| `glfwSetWindowPosCallback` | `window_position_handler` and `set_window_position_handler` | The event is `#glfw_window_position{}`. |
| `glfwSetWindowSizeCallback` | `window_size_handler` and `set_window_size_handler` | The event is `#glfw_window_size{}`. |
| `glfwSetWindowCloseCallback` | `window_close_handler` and `set_window_close_handler` | The event is `#glfw_window_close{}`. |
| `glfwSetWindowRefreshCallback` | `window_refresh_handler` and `set_window_refresh_handler` | The event is `#glfw_window_refresh{}`. |
| `glfwSetWindowFocusCallback` | `window_focus_handler` and `set_window_focus_handler` | The event is `#glfw_window_focus{}`. |
| `glfwSetWindowIconifyCallback` | `window_iconify_handler` and `set_window_iconify_handler` | The event is `#glfw_window_iconify{}`. |
| `glfwSetWindowMaximizeCallback` | `window_maximize_handler` and `set_window_maximize_handler` | The event is `#glfw_window_maximize{}`. |
| `glfwSetFramebufferSizeCallback` | N/A | N/A XXX: Document why it's N/A |
| `glfwSetWindowContentScaleCallback` | `window_content_scale_handler` and `set_window_content_scale_handler` | `#glfw_window_content_scale{}` |
| `glfwPollEvents` | `poll_events` | It causes handlers to be sent events. |
| `glfwWaitEvents` | N/A | Not idiomatic, conflict thread-safety of the binding. Easy to reproduce a similar behavior. |
| `glfwWaitEventsTimeout` | N/A | Not idiomatic, conflict thread-safety of the binding. Easy to reproduce a similar behavior. |
| `glfwPostEmptyEvent` | `post_empty_event` | N/A |
| `glfwSwapBuffers` | N/A | Use `egl:egl:make_current/4` instead. XXX: Consider implementing this as dirty NIF ? |

XXX: Implementation of the window related handler must be checked.

**Monitor**

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwGetMonitors` | `monitors` | We can't distinguish between no monitor or error. Therefore a `[]` return value might mean error. Use `get_error/0` to distinguish. |
| `glfwGetPrimaryMonitor` | `primary_monitor` | Same. We can't distinguish between no monitor or error. Therefore a `undefined` return value might mean error. |
| `glfwGetMonitorPos` | `monitor_position` | N/A |
| `glfwGetMonitorWorkarea` | `monitor_work_area` | N/A |
| `glfwGetMonitorPhysicalSize` | `monitor_physical_size` | N/A |
| `glfwGetMonitorContentScale` | `monitor_content_scale` | N/A |
| `glfwGetMonitorName` | `monitor_name` | N/A |
| `glfwSetMonitorUserPointer` | N/A | N/A |
| `glfwGetMonitorUserPointer` | N/A | N/A |
| `glfwSetMonitorCallback` | `monitor_handler` and `set_monitor_handler` | The event is `#glfw_monitor{}`. XXX: To be tested |
| `glfwGetVideoModes` | `video_modes` | N/A |
| `glfwGetVideoMode` | `video_mode` | N/A |
| `glfwSetGamma` | `set_gamma` | N/A |
| `glfwGetGammaRamp` | `gamma_ramp` | XXX: Verify implementation |
| `glfwSetGammaRamp` | `set_gamma_ramp` | XXX: Verify implementation |

**Input**

| GLFW                          | Binding                                                     | Notes                     |
|------------------------------ | ----------------------------------------------------------- | ------------------------- |
| `glfwGetInputMode`         *   | `input_mode`                                                | N/A                       |
| `glfwSetInputMode`         *   | `set_input_mode`                                            | N/A                       |
| `glfwRawMouseMotionSupported`* | `raw_mouse_motion_supported`                                | N/A                       |
| `glfwGetKeyName`           *   | `key_name`                                                  | N/A                       |
| `glfwGetKeyScancode`       *   | `key_scancode`                                              | N/A                       |
| `glfwGetKey`               *   | `key`                                                       | N/A                       |
| `glfwGetMouseButton`       *   | `mouse_button`                                              | N/A                       |
| `glfwGetCursorPos`         *   | `cursor_position`                                           | N/A                       |
| `glfwSetCursorPos`         *   | `set_cursor_position`                                       | N/A                       |
| `glfwCreateCursor`         *   | `create_cursor`                                             | N/A                       |
| `glfwCreateStandardCursor` *   | `create_standard_cursor`                                    | N/A                       |
| `glfwDestroyCursor`       *    | `destroy_cursor`                                            | N/A                       |
| `glfwSetCursor`          *     | `set_cursor`                                                | N/A                       |
| `glfwSetKeyCallback`      *    | `key_handler` and `set_key_handler` | The event is `#glfw_key{}`. |
| `glfwSetCharCallback`     *    | `char_handler` and `set_char_handler` | The event is `#glfw_char{}`. |
| `glfwSetCharModsCallback` *    | `char_mods_handler` and `set_char_mods_handler` | The event is `#glfw_char_mods{}`. |
| `glfwSetMouseButtonCallback`*  | `mouse_button_handler` and `set_mouse_button_handler` | The event is `#glfw_mouse_button{}`. |
| `glfwSetCursorPosCallback`  *  | `cursor_position_handler` and `set_cursor_position_handler` | The event is `#glfw_cursor_position{}`. |
| `glfwSetCursorEnterCallback`*  | `cursor_enter_handler` and `set_cursor_enter_handler` | The event is `#glfw_cursor_enter{}`. |
| `glfwSetScrollCallback` *      | `scroll_handler` and `set_scroll_handler` | The event is `#glfw_scroll{}`. |
| `glfwSetDropCallback` *        | `drop_handler` and `set_drop_handler` | The event is `#glfw_drop{}`. |

XXX: Implementaiton of the window related handler must be checked.

**Input (Joystick/Gamepad)**

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwJoystickPresent` | `joystick_present` | N/A |
| `glfwGetJoystickAxes` | `joystick_axes` | N/A |
| `glfwGetJoystickButtons` | `joystick_buttons` | N/A |
| `glfwGetJoystickHats` | `joystick_hats` | XXX: Value must be unpacked |
| `glfwGetJoystickName` | `joystick_name` | N/A |
| `glfwGetJoystickGUID` | `joystick_guid` | N/A |
| `glfwSetJoystickUserPointer` | N/A | N/A |
| `glfwGetJoystickUserPointer` | N/A | N/A |
| `glfwJoystickIsGamepad` | `joystick_is_gamepad` | N/A |
| `glfwSetJoystickCallback` | `joystick_handler` and `set_joystick_handler` | The event is `#glfw_joystick{}`. |
| `glfwUpdateGamepadMappings` | `update_gamepad_mappings` | XXX: Verify implementation. |
| `glfwGetGamepadName` | `gamepad_name` | N/A                                   |
| `glfwGetGamepadState` | `gamepad_state` | N/A |

**Input (others)**

| GLFW | Binding | Notes |
|----- | ------- | ----- |
| `glfwGetTime` | N/A | N/A |
| `glfwSetTime` | N/A | N/A |
| `glfwGetTimerValue` | N/A | N/A |
| `glfwGetTimerFrequency` | N/A | N/A |
| `glfwSetClipboardString` | `set_clipboard_string` | XXX: must allocated array dyn |
| `glfwGetClipboardString` | `clipboard_string` | N/A |

**Vulkan-related functions**

All those functions are not implemented as not application.

- `glfwVulkanSupported`
- `glfwGetRequiredInstanceExtensions`
- `glfwGetInstanceProcAddress`
- `glfwGetPhysicalDevicePresentationSupport`
- `glfwCreateWindowSurface`

Also note that the init hint is not blabla XXX

**Native functions**

All those functions are not implemented as not application.

> Perhaps some of them will be implemented if needs for platform-specific
bindings arise later.

- `glfwGetWin32Adapter`
- `glfwGetWin32Monitor`
- `glfwGetWin32Window`
- `glfwGetWGLContext`
- `glfwGetCocoaMonitor`
- `glfwGetCocoaWindow`
- `glfwGetCocoaView`
- `glfwGetNSGLContext`
- `glfwGetX11Display`
- `glfwGetX11Adapter`
- `glfwGetX11Monitor`
- `glfwGetX11Window`
- `glfwSetX11SelectionString`
- `glfwGetX11SelectionString`
- `glfwGetGLXContext`
- `glfwGetGLXWindow`
- `glfwGetWaylandDisplay`
- `glfwGetWaylandMonitor`
- `glfwGetWaylandWindow`
- `glfwGetEGLDisplay`
- `glfwGetEGLContext`
- `glfwGetEGLSurface`
- `glfwGetOSMesaColorBuffer`
- `glfwGetOSMesaDepthBuffer`
- `glfwGetOSMesaContext`

----

change mods to bitfield ? XXX

- All Vulkan-related parts are not exposed (and automatically disabled by
default).
  - The `GLFW_X11_XCB_VULKAN_SURFACE` init hint is not implemented.

- the ` 	glfwGetFramebufferSize ` is not implemented

- Timer stuff not implemented (see input part)

- window_size_limits and window_aspect_ratio  accepts 'dont_care' for convenience
(xxx:document slightly different interface).

- check if key_name should restrict to "printable character" and perhaps implement a funciton to check if it is.


- double-check init hints implementation - especially about the "default" value
 (should we support them ?)

- primary monitor/0 and monitors/0 function will return different handles. for
instnace calling primary_monitor/0 multiple times should really always return
the same handle.

- check window title and utf8 thing

- implement window monitor and set_window_monitor functions

- implementation of key/2 function: consider renaming to window_key/2  ? also
many keys are not allowed, perhaps rewrite definition in order to distinguish "printable"keys, "modifier keys", etc.
