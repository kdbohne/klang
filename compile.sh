#/bin/sh
./bin/klang test/first.k | llc -O0 -filetype=obj -o tmpobj.o
clang tmpobj.o -o ./bin/first
rm tmpobj.o
