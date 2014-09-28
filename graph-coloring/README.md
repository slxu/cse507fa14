This folder contains a solution skeleton for Problem 9 of Homework
1 for CSE 507. The skeleton is implemented in the Racket programming lanaguage,
which can be obtained from http://racket-lang.org.  Your task is to implement 
 the [k-coloring](src/k-coloring.rkt) procedure, which uses a SAT solver to 
find a k-coloring for a graph (if one exists).

To complete the homework:

* Download and compile
the latest version of the
[Lingeling](http://fmv.jku.at/lingeling/) and, optionally,
[Glucose](http://www.labri.fr/perso/lsimon/glucose/) SAT solver.  The resulting 
binaries should go into the `graph-coloring/bin/` directory.

* Download the [all-instances archive](https://sites.google.com/site/graphcoloring/files/all-instaces.tar.gz) from the [Graph Coloring Benchmarks](https://sites.google.com/site/graphcoloring/) page, and place its contents into the `graph-coloring/data/` directory.

* See the [examples.rkt](src/examples.rkt) file for a quick tour of the types and procedures to use in your implementation.