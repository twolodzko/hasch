
The code example comes from _*The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme, my implementation of Scheme in [Go (`gosch`)](https://github.com/twolodzko/gosch)
and [OCaml (`loco`)](https://github.com/twolodzko/loco).

```shell
$ hyperfine -m 100 --warmup 10 \
    'gosch run-all.scm' \
    'scheme --quiet < run-all.scm' \
    'loco run-all.scm' \
    '../../../rusch/rusch run-all.scm' \
    '../../hasch run-all.scm'
Benchmark 1: gosch run-all.scm
  Time (mean ± σ):      94.4 ms ±   7.3 ms    [User: 107.1 ms, System: 9.6 ms]
  Range (min … max):    82.5 ms … 135.0 ms    100 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     263.4 ms ±   8.0 ms    [User: 207.0 ms, System: 56.2 ms]
  Range (min … max):   252.9 ms … 296.7 ms    100 runs
 
Benchmark 3: loco run-all.scm
  Time (mean ± σ):      24.7 ms ±   2.5 ms    [User: 22.8 ms, System: 2.0 ms]
  Range (min … max):    21.3 ms …  33.1 ms    100 runs
 
Benchmark 4: ../../../rusch/rusch run-all.scm
  Time (mean ± σ):      22.6 ms ±   2.1 ms    [User: 22.0 ms, System: 0.8 ms]
  Range (min … max):    20.4 ms …  28.9 ms    107 runs
 
Benchmark 5: ../../hasch run-all.scm
  Time (mean ± σ):      36.5 ms ±   2.9 ms    [User: 33.3 ms, System: 3.2 ms]
  Range (min … max):    32.9 ms …  44.1 ms    100 runs
 
Summary
  '../../../rusch/rusch run-all.scm' ran
    1.09 ± 0.15 times faster than 'loco run-all.scm'
    1.61 ± 0.19 times faster than '../../hasch run-all.scm'
    4.17 ± 0.50 times faster than 'gosch run-all.scm'
   11.63 ± 1.12 times faster than 'scheme --quiet < run-all.scm'
```
