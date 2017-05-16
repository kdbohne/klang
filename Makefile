BINARY_NAME=klang
BINARY_DIR=bin
BUILD_DIR=build

COMPILER=clang
COMPILER_FLAGS=-m64 -std=c++11 -O0 -g -DPLATFORM_LINUX \
			   -Isrc \
			   -fno-exceptions -fno-rtti \
			   -Werror -Wall -Wpedantic -Wshadow \
			   -Wno-missing-braces -Wno-unused-function -Wno-unused-parameter -Wno-unused-variable
LINKER_FLAGS=-m64 -fuse-ld=gold -Lbin

SOURCES:=$(wildcard src/*.cpp)
HEADERS:=$(wildcard src/*.h)
OBJECTS:=$(patsubst src/%.cpp, $(BUILD_DIR)/%.o, $(SOURCES))

.PHONY: default
default: main

main: $(OBJECTS)
	@mkdir -p bin
	@$(COMPILER) $(COMPILER_FLAGS) $(LINKER_FLAGS) $(OBJECTS) -o $(BINARY_DIR)/$(BINARY_NAME)

$(BUILD_DIR)/%.o: src/%.cpp $(HEADERS)
	@mkdir -p build
	@$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@

.PHONY: clean
clean:
	@rm -rf $(BINARY_DIR) $(BUILD_DIR)
