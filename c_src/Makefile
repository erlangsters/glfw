ERTS_INCLUDE_DIR ?= $(ERLANG_ERTS_DIR)/include
ERL_INTERFACE_INCLUDE_DIR ?= $(ERL_INTERFACE_DIR)/include
ERL_INTERFACE_LIB_DIR ?= $(ERL_INTERFACE_DIR)/lib

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c17 -Wall -Wextra -Werror=implicit-function-declaration
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c17 -Wall -Wextra -Werror=implicit-function-declaration
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lei
LDFLAGS += -shared

ifeq ($(UNAME_SYS), Darwin)
	CFLAGS += -I $(ANGLE_INCLUDE_DIR)
	LDLIBS += -L $(ANGLE_LIB_DIR)
endif

ifeq ($(UNAME_SYS), Darwin)
	CFLAGS += $(shell pkg-config --cflags glfw3)
	LDLIBS += $(shell pkg-config --libs glfw3)
else ifeq ($(UNAME_SYS), Linux)
	LDLIBS += -lglfw
endif

SOURCES = glfw.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = ../priv/beam-glfw.so

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJECTS)
	@mkdir -p ../priv
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LDLIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(TARGET)
