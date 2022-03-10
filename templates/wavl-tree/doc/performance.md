### Performance Results

Current results of `perf_test`, which times the insertion of 1M random values
(from a range of 100M values), lookup of 1M different random values (from the
same range), and sequential deletion of the values originally inserted.

* AMD Threadripper 2920X

  compiler | flags | insertion | lookup | deletion |
  ---------|-------|-----------|--------|----------|
  nagfor 7.1.7103 | -O3 | 0.45 s | 0.50 s | 0.50 s
  gfortran 11.2.0 | -O2 | 0.57 s | 0.59 s | 0.62 s
  ifort 2021.5.0  | -O2 | 0.67 s | 0.65 s | 0.72 s

  - Significant improvement for nagfor increasing optimization to -O3;
    no change for gfortran and ifort.
