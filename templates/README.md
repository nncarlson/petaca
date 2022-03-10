## Templates

This directory contains modules that provide reference implementations of
various data structures. They are not intended to be used as is, but rather
to serve as starting code for developing application-specific code that will
use the data structures. Ideally these would be true templates, but Fortran
lacks capabilities for generic programming[^1].

### Contents
* [`wavl-tree`](./wavl-tree): A weak AVL binary search tree. WAVL trees are
self-balancing binary search trees that combine the best properties of AVL
and red-black trees.

[^1] There are pre-processing options for implementing a form of generic
programming. See for example, the
[Fortran Standard Library](https://github.com/fortran-lang/stdlib)
and its use of the [`fypp`](https://github.com/aradi/fypp) pre-processor,
or Tom Clune's [gFTL](https://github.com/Goddard-Fortran-Ecosystem/gFTL)
library and its use of the GNU `m4` macro processor. I'd like to explore
these in the future.
