This folder contains a solution skeleton for Problem 9 of Homework
1 for CSE 507. The skeleton is implemented in the Racket programming lanaguage,
which can be obtained from http://racket-lang.org.

To complete the homework, you will need to download and compile
the latest version of the
[Lingeling](http://fmv.jku.at/lingeling/) and, optionally,
[Glucose](http://www.labri.fr/perso/lsimon/glucose/) SAT solver.  The resulting 
binaries should go into the `graph-coloring/bin/` directory.

You will also need to download the [all-instances archive](https://sites.google.com/site/graphcoloring/files/all-instaces.tar.gz) from the [Graph Coloring Benchmarks](https://sites.google.com/site/graphcoloring/) page, and place its contents into the `graph-coloring/data/` directory.

With this in place, see the [examples.rkt](src/examples.rkt) file for a quick tour of the data-structures and procedures you may want to use to implement the [k-coloring](src/k-coloring.rkt) encoding. 