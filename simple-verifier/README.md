This folder contains a solution skeleton for Problem 5 of Homework
2 for CSE 507. The skeleton is implemented in Racket,
which can be obtained from http://racket-lang.org.  Your task is to implement 
 the [verify](src/verifier.rkt) procedure, which uses an SMT solver to 
check equivalence of  loop-free instruction sequences.

To complete the homework:

* Create  a `bin` subfolder in your copy of `cse507fa14/simple-verifier/`.

* Download and compile
the latest version of the
[Z3](http://z3.codeplex.com) SMT solver.  Place the resulting 
binary into `cse507fa14/simple-verifier/bin/`.  Alternatively, place the binary in a 
folder of your choosing and adjust the `z3` path variable as shown 
in [examples.rkt](src/examples.rkt).

* See [src/examples.rkt](src/examples.rkt) for a quick tour of the types and procedures to use in your implementation.
