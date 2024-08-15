# Random Number Implementation and Validation

Compilable code is build with:

```shell
cmake -S . -B cmake-build
cmake --build cmake-build
```

Some analysis is performed in the [jupyter] folder.


## TODOs

* Move reservoir sampling code from desktop to here
* Move wrapped GSL examples? here
* Test reservoir sampling 
  * Distribution of numbers
  * speed
* Test hidden shuffle
  * Distribution of numbers
  * speed


## PRNGs

Implemented [XOR](src/fortran/xorshifts.f90), which is straightforward and has a large period of 
over a million. Also showed that mapping $[0, P)$ to $[a, b)$ is fine when the values are real
but mapping to a smaller range of integers will inevitably result in duplication of numbers,
even when uniformly sampling.

See:
* [Lemire's mapping](src/cpp/lemire_mapping.cpp)
* Mapping in the [XOR](src/fortran/xorshifts.f90) module


## Random Sampling a Population with no Replacements

For my use cases, one requires random sampling with no replacement.
Algorithms that randomly sample a population with no replacements include:

* Reservoir sampling
	* A couple of versions are shown on [wikipedia](https://en.wikipedia.org/wiki/Reservoir_sampling)
    * My [simple implementation](src/fortran/reservoir_sampling.f90)

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