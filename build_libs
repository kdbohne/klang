#!/bin/sh

STD="-std=c99"
FLAGS="-Wall -Wextra -Wpedantic -Wshadow -Wno-unused-parameter -Wno-unused-variable"

DIR="lib"
BUILD_DIR="$DIR/build"

mkdir -p lib/build
clang $STD -m64 -c lib/gl.c -o $BUILD_DIR/gl.o $FLAGS
clang $STD -m64 -c lib/dll.c -o $BUILD_DIR/dll.o $FLAGS

clang -m64 -c $DIR/start.s -o $BUILD_DIR/start.o
clang -m64 -c $DIR/syscall.s -o $BUILD_DIR/syscall.o
clang $STD -m64 -c $DIR/bit.c -o $BUILD_DIR/bit.o
