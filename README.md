This benchmark is born to compare the performance of parallel language/runtime systems.
It comes after the sequential kmeans benchmark:
https://github.com/andreaferretti/kmeans

Rules
=====

The implementations should all follow the same algorithm, and be optimized for idiomatic code and not for speed. The example is intended to compare time of execution for a typical machine learning algorithm, ideally during an interactive session, instead of highly optimized production code. As such, it is important that the code is straightforward and that there is no separate phase to prepare the caches.

The points are in `points.json`, and are to be grouped into 10 clusters, using 15 iterations of kmeans. The initial centroids are initialized to the first 10 points, and we take an average over 100 runs.

Contribute
==========

If you want to contribute an implementation in a different language, please file a PR. Try to follow the same logic that is used in the examples in other languages.
The intended logic steps are:
  - calculate closest centroid for each point trying to maximize parallelism
  - sync
  - calculate new centroids value
  - sync
As you may notice, the algorithm is not optimized, and intentionally so: while K-means in particular has various possible optimizations, other similar algorithms may fail to have the particular shape that makes these optimizations viable.
Hard code a flag to print final centroids at the last step.

Please within contribution write a couple of lines on the dependencies to install and provide a proper Makefile that do the follows:
```
clean: delete all temporary/compiled files
all: compile to an executable
run: run the program
```

How to compile
==========

**Akka**

    install sbt: http://www.scala-sbt.org/release/tutorial/Setup.html

**Erlang**

    install erlang: https://www.erlang-solutions.com/downloads

**Pony**

    install ponyc: http://tutorial.ponylang.org/getting-started/installation/
