# Thread Safety

GLFW is not "thread-safe". However, good news, this binding is entirely 
thread-safe. Now, what does that mean at the BEAM level where "threads" do not 
even exist? It means you can execute any GLFW function anywhere you want in 
your code.

> Don't be confused with the thread safety of the OpenGL bindings. While GLFW 
> binding is thread safe at the BEAM level, the OenGL bindings are not.

## Why not thread safe ?

The GLFW library is not thread-safe and for good reasons; it's close to impossible to
make a library like this thread-safe. It does the very difficult job of unifying
all platform-specifics into a single API.

If we dig into what happens under the hood, you will find that
blablab
blabla

It's the reason that the GLFW library mandates that almost all its functions 
are executed from the main thread if you do not want to experience 
platform-specific issue.

> Indeed, a piece of multi-threaded GLFW code might run fine on one platform 
> and fail miserably on another one.

If all functions are executed from the main thread, GLFW guaranteed it will 
work.

## How thread safety is achieved ?

Since GLFW is meant to be used on the main thread, or at least, from a single 
thread where the `glfwInit()` called is made, the binding creates a separate OS 
thread and schedule the execution of the GLFW calls on it.

When a GLFW function is marked as thread-safe, the binding directly executes it.
When a GLFW function is not marked as thread-safe, the binding forward it to 
the separate OS thread.

This mechnism ensures "thread-safety" at the BEAM level.


## Implications

You do not have to worry 

talk about callback
