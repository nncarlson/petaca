# Module co_td_matrix_type

This module defines the derived type `co_td_matrix` which is a parallel data
structure for tridiagonal matrices implemented using Fortran coarrays. The
derived type includes direct linear solver and matrix-vector product methods.

In this implementation the rows of the matrix (and corresponding variables) are
block-partitioned with successive blocks stored in successive images. The last
row of each block (except the last), which couples variables in adjacent images,
are *boundary* equations. The remaining equations are *interior* equations.
To compute an LU factorization of the matrix, the interior equations are first
eliminated. This is done in parallel requiring no communication between images.
This leaves a reduced tridiagonal Schur complement system involving just the
boundary variables (one per image) whose LU factorization is computed in serial.

## Status
This is a work-in-progress.
* It currently works with the NAG 7.1 compiler;
* It is not working with either the Intel ifort or gfortran/opencoarray
  compilers; the linear solver generates grossly incorrect results.
* Except for extremely large systems (> 1M), increasing the number of
  images actually increases the runtime rather than decreasing it.


## Compiling and Testing

From the directory containing this README file:
```sh
$ mkdir build
$ cd build
$ cmake ..
$ make
$ ctest
```
The build system understands the NAG, Intel ifort, and gfortran/opencoarrays
compilers. You may need to set your `FC` environment variable to the path to
your compiler. For gfortran you must set `FC=caf` (provided by opencoarrays).

## Licence
This project is distributed under the terms of the MIT license.