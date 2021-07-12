# ICFP Programming Contest 2021

This is Team Sampou's repository for the [ICFP Programming Contest 2021](https://icfpcontest2021.github.io/).

## Programming Languages used

* Haskell

## Approach

Our approach includes:

* Solving by hands :upside_down_face:
* Solving using SMT solver [Z3]()
  * We encode the problems of finding feasible solution as `QF_LIRA` SMT problems and invoked [Haskell binding](https://hackage.haskell.org/package/z3) to solve them.
  * Coordinates of points and differences between points are represented as integer variables. 
  * Since (-)² cannot be represented in `QF_LIRA`, we represent *x²* as a distinct variable and relationship between *x* and *x²* is enforced by a set of constraints.
  * We initially add constraints about edge length and constrains that vertexes are contained in the hole.
  * If the hole is not convex, we may get a solution where the edge goes outside the hole in the middle.
    In such a case, we add a constraint on demand like CEGAR, that the hole contains a point that divides the edge in an appropriate ratio.


