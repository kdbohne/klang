#!/bin/bash

# TODO: check argument count
FILENAME=$(echo $1 | cut -f 1 -d '.' | rev | cut -f 1 -d '/' | rev)

IR="$(./bin/klang $1)"
printf "%s" "$IR" | clang -x c - -m64 -c -o $FILENAME.o

# TODO: detect libs in frontend
OBJS="lib/build/start.o lib/build/syscall.o lib/build/bit.o lib/build/dll.o lib/build/gl.o"
LIBS="-ldl -lX11 -lGL"

# TODO: -nostdlib?
clang -m64 -nostartfiles $FILENAME.o $OBJS -o ./bin/$FILENAME $LIBS
rm $FILENAME.o
