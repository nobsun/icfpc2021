# ICFP Programming Contest 2021

This is Team Sampou's repository for the [ICFP Programming Contest 2021](https://icfpcontest2021.github.io/).

## Programming Languages used

* Haskell

## Approach

Our approach includes:

* Solving by hands using GUI :upside_down_face:
  * GUI is implemented using [OpenGL](https://hackage.haskell.org/package/OpenGL) using [GLFW-b](https://hackage.haskell.org/package/GLFW-b)
* Solving using SMT solver [Z3](https://github.com/Z3Prover/z3)
  * We encode the problems of finding feasible solution as `QF_LIRA` SMT problems and invoked [Haskell binding](https://hackage.haskell.org/package/z3) to solve them.
  * Coordinates of points and differences between points are represented as integer variables. 
  * Since (-)² cannot be represented in `QF_LIRA`, we represent *x²* as a distinct variable and relationship between *x* and *x²* is enforced by a set of constraints.
  * We initially add constraints about edge length and constrains that vertexes are contained in the hole.
  * If the hole is not convex, we may get a solution where the edge goes outside the hole in the middle.
    In such a case, we add a constraint on demand like CEGAR, that the hole contains a point that divides the edge in an appropriate ratio.

## Members

* [Katsutoshi Itoh](https://github.com/cutsea110)
* [Naoki Iwayama](https://github.com/Hogeyama)
* [Kei Hibino](https://github.com/khibino)
* [Yasuyuki Ogawa](https://github.com/oganet)
* [Masahiro Sakai](https://github.com/msakai)
* [Nobuo Yamashita](https://github.com/nobsun)
