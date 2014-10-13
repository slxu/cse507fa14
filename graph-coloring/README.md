This folder contains a solution skeleton for Problem 9 of [Homework
1](http://courses.cs.washington.edu/courses/cse507/14au/hws/hw1.pdf) for [CSE 507](http://courses.cs.washington.edu/courses/cse507/14au/index.html). The skeleton is implemented in Racket,
which can be obtained from http://racket-lang.org.  Your task is to implement 
 the [k-coloring](src/k-coloring.rkt) procedure, which uses a SAT solver to 
find a k-coloring for a graph (if one exists).

To complete the homework:

* Create `bin` and `data` subfolders in your copy of `cse507fa14/graph-coloring/`.

* Download and compile
the latest version of the
[Lingeling](http://fmv.jku.at/lingeling/) and, optionally,
[Glucose](http://www.labri.fr/perso/lsimon/glucose/) SAT solver.  Place the resulting 
binaries into `cse507fa14/graph-coloring/bin/`.

* Download the [all-instances archive](https://sites.google.com/site/graphcoloring/files/all-instaces.tar.gz) from the [Graph Coloring Benchmarks](https://sites.google.com/site/graphcoloring/) page, and place its contents into `cse507fa14/graph-coloring/data/`.

* See [src/examples.rkt](src/examples.rkt) for a quick tour of the types and procedures to use in your implementation.
