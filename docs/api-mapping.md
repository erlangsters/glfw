# API Mapping

It's binding of an existing library and therefore knowledge of GLFW itself is 
expected. 



> As much as possible, minimal API transformations were applied.

Other than the contextless window particularity and the thread-safety concerns
that are solved at the binding-level, 100% of your existing GLFW knowledge is
transferable with a few notes.

- All Vulkan-related parts are not exposed (and automatically disabled by
default).
  - The `GLFW_X11_XCB_VULKAN_SURFACE` init hint is not implemented.
  - The `glfwInitVulkanLoader` function is not implemented.

- No "custom heap memory allocator" possible. It does not make sense.
  - The `glfwInitAllocator` function is not implemented.

- pointer lifetime.

- All platform "native" functions that's provided by GLFW are not implemented.
Not even the "EGL ones".

- the ` 	glfwGetFramebufferSize ` is not implemented

- xxx: Normally, glfwInit is called once. howeverot too sure about terminate/init


If ther'es still ambiguisty, contact us.


----



- The `getError` function was implemeted as `last_error/0`

- The error callback functionality (the `glfwSetErrorCallback` function) is
implemented as an event handler. A process that receives the 'error' events
is set with the `gflw:set_error_handler/1` function.
Also, when replacing the error handler, instead of returning the current
handler, the `gflw:error_handler/1` function was implemented (no equivalent in
the GLFW C API).

- The `gflw:error_handler` function was added to retrive the current handler


- Timer stuff not implemented (see input part)

- Technically the value of "window should close" is an int. But in this binding
it's a boolean. I undersatnd it can be a feature, however, then we would have to
deal with int bit size (erlang int is not constrainted by bit size). - it would
overcomplicate the impl/interface.


- window_size_limits and window_aspect_ratio  accepts 'dont_care' for convenience
(xxx:document slightly different interface).

- check if key_name should restrict to "printable character" and perhaps implement a funciton to check if it is.

TODO:

-rename to `glfw-3`. for glfw 4 we would fork and create different repo.


- double-check init hints implementation - especially about the "default" value
 (should we support them ?)

- primary monitor/0 and monitors/0 function will return different handles. for
instnace calling primary_monitor/0 multiple times should really always return
the same handle.

- when glfw is not init yet, functions like monitor name and primary monitor will return NULL. Should it translate to
undefined atom  ?

- check window title and utf8 thing

- implement window monitor and set_window_monitor functions

- implementation of key/2 function: consider renaming to window_key/2  ? also
many keys are not allowed, perhaps rewrite definition in order to distinguish "printable"keys, "modifier keys", etc.


**Monitor-related notes**

- monitorcallback was replaced with "handler" same fashion as error handler.


- All context-related functions are not implemented. They're not essential as
you can achieve the same results with the EGL and OpenGL bindings.


- bar

To be written.