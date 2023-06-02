# Module td_matrix_type
This module defines the derived type `btd_matrix` which is a data structure
for block tridiagonal matrices. The derived type includes direct linear solver
and matrix-vector product methods, and provides support for periodic block
tridiagonal matrices as well.

## Compiling and Testing
From the directory containing this README file:
```
$ mkdir build
$ cd build
$ cmake ..
$ make
$ ctest
```
You may need to set your `FC` environment variable to the path to your compiler
prior to running `cmake`.

This works with the NAG, GFortran, and Intel oneAPI classic ifort and
LLVM-based ifx compilers.

The test problem `test/btd_matrix_test.f90` also serves as a usage example.

## Licence
This project is distributed under the terms of the MIT license.