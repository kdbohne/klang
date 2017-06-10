#/bin/sh
./bin/klang test/first.k | llc -O0 -filetype=obj -o build/first.o

# Compile core utils
clang -m64 -c lang/core/start.s -o build/start.o
clang -m64 -c lang/core/syscall.s -o build/syscall.o

clang -m64 -nostartfiles build/first.o build/start.o build/syscall.o -o ./bin/first
#clang -m64 -nostartfiles -nostdlib build/first.o build/start.o build/syscall.o -o ./bin/first

#rm build/first.o build/syscall.o
