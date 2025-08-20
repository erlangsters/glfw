cmake_minimum_required(VERSION 3.10)
project(beam_glfw LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

# We let Rebar3 inform CMake where to find the Erlang/OTP installation.
# Therefore, it requires the ERLANG_ERTS_DIR and ERL_INTERFACE_DIR values
# specified with the -D flags.
if(NOT DEFINED ERLANG_ERTS_DIR)
    message(FATAL_ERROR "ERLANG_ERTS_DIR must be defined (pass -DERLANG_ERTS_DIR=...)")
endif()
if(NOT DEFINED ERL_INTERFACE_DIR)
    message(FATAL_ERROR "ERL_INTERFACE_DIR must be defined (pass -DERL_INTERFACE_DIR=...)")
endif()

# On macOS and Windows, it also requires the ANGLE_INCLUDE_DIR and
# ANGLE_LIB_DIR values specified with the -D flags.
if(APPLE OR WIN32)
    if(NOT DEFINED ANGLE_INCLUDE_DIR)
        message(FATAL_ERROR "ANGLE_INCLUDE_DIR must be defined (pass -DANGLE_INCLUDE_DIR=...)")
    endif()
    if(NOT DEFINED ANGLE_LIB_DIR)
        message(FATAL_ERROR "ANGLE_LIB_DIR must be defined (pass -DANGLE_LIB_DIR=...)")
    endif()
endif()

set(ERTS_INCLUDE_DIR "${ERLANG_ERTS_DIR}/include")
set(ERL_INTERFACE_INCLUDE_DIR "${ERL_INTERFACE_DIR}/include")
set(ERL_INTERFACE_LIB_DIR "${ERL_INTERFACE_DIR}/lib")

include_directories(
    ${ERTS_INCLUDE_DIR}
    ${ERL_INTERFACE_INCLUDE_DIR}
)
link_directories(${ERL_INTERFACE_LIB_DIR})

# If ANGLE is used, we also need to include its directories.
if(APPLE OR WIN32)
    include_directories(${ANGLE_INCLUDE_DIR})
    link_directories(${ANGLE_LIB_DIR})
    # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DANGLE_ENABLED")
else()
    # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DANGLE_DISABLED")
endif()

set(BEAM_GLFW_SOURCES
    c_src/glfw.c
    c_src/command_executor.c
)
add_library(beam-glfw SHARED ${BEAM_GLFW_SOURCES})

set_target_properties(beam-glfw PROPERTIES
    OUTPUT_NAME "beam-glfw"
    PREFIX ""
)

# The Rebar3 build system (as configured in rebar.config) does not make use
# of the "install" step. Instead, it produces the NIF library in the priv/
# folder directly during the "build" step.
set_target_properties(beam-glfw PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
)

# On macOS, we need to add special linker flags to avoid issues with symbol
# resolution.
if(APPLE)
    set_target_properties(beam-glfw PROPERTIES
        LINK_FLAGS "-flat_namespace -undefined suppress"
    )
endif()

target_link_libraries(beam-glfw PRIVATE ei)

# Link against EGL.
if(WIN32)
    # On Windows, link with ANGLE's libEGL
    target_link_libraries(beam-glfw PRIVATE libEGL)
elseif(APPLE)
    # On macOS, link with ANGLE's libEGL
    target_link_libraries(beam-glfw PRIVATE EGL)
else()
    # On Linux, link with system EGL
    target_link_libraries(beam-glfw PRIVATE EGL)
endif()

# Link against GLFW.
find_package(glfw3 CONFIG REQUIRED)
if(NOT glfw3_FOUND)
    find_path(GLFW_INCLUDE_DIR NAMES GLFW/glfw3.h)
    find_library(GLFW_LIBRARY NAMES glfw3 glfw)
    if(GLFW_INCLUDE_DIR AND GLFW_LIBRARY)
        include_directories(${GLFW_INCLUDE_DIR})
        target_link_libraries(beam-glfw PRIVATE ${GLFW_LIBRARY})
        message(STATUS "Found GLFW: ${GLFW_INCLUDE_DIR}, ${GLFW_LIBRARY}")
    else()
        message(FATAL_ERROR "GLFW not found. Please install GLFW or set GLFW_INCLUDE_DIR and GLFW_LIBRARY.")
    endif()
else()
    target_link_libraries(beam-glfw PRIVATE glfw)
    message(STATUS "Using glfw3 from find_package")
endif()
