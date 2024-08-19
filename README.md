# Random Number Implementation and Validation

Compilable code is build with:

```shell
cmake -S . -B cmake-build
cmake --build cmake-build
```

Some analysis is performed in the [jupyter] folder. This is done by automatically wrapping the fortran shared library
with the fantastic [gfort2py](https://github.com/rjfarmer/gfort2py).

## TODOs

**Sorting**

- [ ] Migrate wrapped GSL C calling example here
- [ ] Add a pytoml, such that one can straightforwardly install the python dependencies

**Mapping a random number to an interval**

- [ ] Test Lemire's algorithm in the [integer mapping module](src/fortran/integer_mapping.f90)
  * Note, there _could_ be issues arising from transcribing from C to fortran

**Sampling without replacement**

Test:
- [ ] My choice of random seed precision (`uint`)
- [ ] Hidde shuffle
   * Contains bug/s
- [ ] Time all algorithms tested in the [notebook](jupyter/sampling_without_replacement.ipynb)

Implement
- [ ] Weighted version of [reservoir sampling](https://en.wikipedia.org/wiki/Reservoir_sampling#Weighted_random_sampling)
  * Ideally where the indices and weights are evaluated on-the-fly
  * Also see the paper [Weighted random sampling with a reservoir](https://doi.org/10.1016/j.ipl.2005.11.003)


## PRNGs

Implemented [XOR](src/fortran/xorshifts.f90), which is straightforward and has a large period of over a million. 
However, one has to be careful with the handling of signed  vs unsigned integers when transcribing
from C.

An unsigned int in C can hold numbers $[0, 2^{32} - 1]$, however fortran does not support this data type. 
Instead, signed `int32` has the range $[-2^{31}, 2^{31} - 1]$. I use a bit mask to remap negative values:

```fortran
! A mask that has all the bits set to 1 except the most significant bit 
! i.e. the sign bit in a 32-bit signed integer
iand(x, Z'7FFFFFFF')
```

This leaves $[0, 2^{31}-1]$ unchanged. However, for negative values, $x$ is represented using two's complement notation. 
When you apply `iand(x, Z'7FFFFFFF')`, you are effectively masking out the sign bit. This operation converts a negative 
number into its unsigned equivalent by removing the sign bit and keeping only the lower 31 bits.

```fortran
integer(int32) :: x
x = -12345678_int32  ! x = -12345678 (in two's complement)
x = iand(x, Z'7FFFFFFF')  ! Masking the sign bit
! x becomes 2015137970 (the unsigned equivalent of -12345678)
```

There are smarter things one can do. See this [Github reference](https://github.com/Jonas-Finkler/fortran-xorshift-64-star/blob/main/src/random.f90)
by Jonas Finker, or [MR 2528](https://gitlab.com/octopus-code/octopus/-/merge_requests/2528/) for Octopus, however the 
above is currently sufficient for my needs.


## Mapping integers to a smaller range

Also showed that mapping $[0, P)$ to $[a, b)$ is fine when the values are real
but mapping to a smaller range of integers will inevitably result in duplication of numbers,
even when uniformly sampling.

See:
* [Lemire's mapping](src/cpp/lemire_mapping.cpp)
* Mapping in the [XOR](src/fortran/integer_mapping.f90) module


## Random Sampling a Population with no Replacements

For my use cases, one requires random sampling with no replacement.
Algorithms that randomly sample a population with no replacements include:

* Reservoir sampling
	* A couple of versions are shown on [wikipedia](https://en.wikipedia.org/wiki/Reservoir_sampling)
    * My [implementations](src/fortran/reservoir_sampling.f90)

* Skip and Gap Sampling (Vitter's Algorithm)
	* Can be more efficient than standard Reservoir Sampling, especially for large streams
		* [Original paper](http://www.ittc.ku.edu/~jsv/Papers/Vit84.sampling.pdf)  with algorithms A - D, and followed up [here](http://www.ittc.ku.edu/~jsv/Papers/Vit87.RandomSampling.pdf)
		* [Reservoir Algorithms: Random Sampling with a Reservoir](https://richardstartin.github.io/posts/reservoir-sampling#reservoir-algorithms-random-sampling-with-a-reservoir). This link is quite thorough and covers Algorithms A, D, R, X, Z, L
		* Some more details on Knuth's Algorithm L [here](http://guptamukul.blogspot.com/2009/12/understanding-algorithm-l_05.html)	
	* [Blog post](http://erikerlandson.github.io/blog/2014/09/11/faster-random-samples-with-gap-sampling/) on gap sampling
		* Quite short
		* Touches on Poisson distribution, which is also utilised by hidden shuffle - worth a read, but the code is Java

* [Hidden Shuffle](http://wrap.warwick.ac.uk/150064)  This gives a python implementation, and claims it's more efficient than the above methods
  * My [python implementation](src/python/hidden_shuffle.py), transcribed from the paper
  * My [fortran implementation](src/fortran/hidden_shuffle.f90)

* Hash-Based Sampling

* Simple Random Sampling with Sorting
	* Efficient when the range (m) is small w.r.t. N (i.e. $2^{32}$)
	* Guarantees uniqueness of selected items.

Some overviews on the problem, and related algorithms:
 * Looks like a good, recent [paper](https://arxiv.org/pdf/2104.05091) "Simple, Optimal Algorithms for Random Sampling Without Replacement" giving an overview of the methods listed here
 * For way more detail and code examples, see this [gist](https://peteroupc.github.io/randomfunc.html)

Fortran implementation references:
* [Suite of old apps](https://people.math.sc.edu/Burkardt/f_src/rnglib/rnglib.html)
* [XOR Github reference](https://github.com/Jonas-Finkler/fortran-xorshift-64-star/blob/main/src/random.f90) 
* [MersenneTwister-Lab in C](https://github.com/MersenneTwister-Lab/XSadd)
