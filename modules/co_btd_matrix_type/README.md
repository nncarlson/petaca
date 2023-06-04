# Module co_btd_matrix_type
This module defines the derived type `co_btd_matrix` which is a parallel data
structure for block tridiagonal matrices implemented using Fortran coarrays.
The derived type includes direct linear solver and matrix-vector product
methods, and provides support for periodic block tridiagonal matrices as well.

This uses the same implementation as `co_td_matrix` but with its scalar matrix
elements replaced by (square) block matrices.

## Status
This is a work-in-progress.
* It currently works with these compilers: gfortran 12.1 with opencoarrays 2.1,
  Intel oneAPI classic ifort and the new LLVM-based ifx
* The non-periodic case works with the NAG 7.1 compiler, but there is an bug in
  the latest build of the compiler (7129) that causes the periodic case to fail.
  I expect this to be fixed soon. See
  https://github.com/nncarlson/fortran-compiler-tests/blob/master/nag-bugs/nag-20230603c.f90
* As with `co_td_matrix`, the current parallel performance is disappointing.
  
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

The test problem `test/co_btd_matrix_test.f90` also serves as a usage example.

## Licence
This project is distributed under the terms of the MIT license.