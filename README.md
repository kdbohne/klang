klang
=====

[klang](https://github.com/kdbohne/klang) is a compiler for an experimental programming language.

Building klang
--------------

klang depends on LLVM. With this library installed, simply clone the repository
and run `make`.

```bash
$ git clone https://github.com/kdbohne/klang
$ cd klang
$ make
```

Build the language's core libraries by running the `build_libs` Bash script
located at the root of the repository.

```bash
$ ./build_libs
```

Running klang
-------------

Run the `compile` Bash script from the root of the repository. Supply input
source files as command line arguments. See the `test` directory for example
source code.

```bash
$ ./compile test/window.k
```

The resulting executable is placed in the `bin` directory at the root of the
repository.

License
-------

This source code is provided under the MIT License. The full text of this
license can be found in the `LICENSE.txt` file located in this repository.
