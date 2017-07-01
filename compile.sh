#!/bin/bash

# Compile core utils
clang -m64 -c lang/core/start.s -o build/start.o
clang -m64 -c lang/core/syscall.s -o build/syscall.o
clang -m64 -c lang/core/bit.c -o build/bit.o

# FIXME: pass backend choice to klang
ir=$(./bin/klang test/first.k)
if [ "$?" -ne 0 ] ; then
    exit
fi

if [ "$1" == "c" ] ; then
    echo "$ir" | clang -x c - -m64 -c -o build/first.o
elif [ "$1" == "llvm" ] ; then
    #opt_ir=$(echo "$ir" | opt -O0 -mem2reg)
    #if [ "$?" -ne 0 ] ; then
    #    exit
    #fi

    echo "$ir" | llc -O0 -filetype=obj -o build/first.o
else
    echo "Error: invalid backend specified. [c, llvm]"
    exit
fi

clang -m64 -nostartfiles build/first.o build/start.o build/syscall.o build/bit.o -o ./bin/first
#clang -m64 -nostartfiles -nostdlib build/first.o build/start.o build/syscall.o -o ./bin/first

#rm build/first.o build/syscall.o
