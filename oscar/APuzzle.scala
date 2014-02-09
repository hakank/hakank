package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.core._

/**
 * Number puzzle in Oscar
 *
 * From "God plays dice"
 * "A puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
 * And the sequel "Answer to a puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/
 *
 * This problem instance was taken from the latter blog post.
 * (Problem 1)
 *
 * """
 * 8809 = 6
 * 7111 = 0
 * 2172 = 0
 * 6666 = 4
 * 1111 = 0
 * 3213 = 0
 * 7662 = 2
 * 9312 = 1
 * 0000 = 4
 * 2222 = 0
 * 3333 = 0
 * 5555 = 0
 * 8193 = 3
 * 8096 = 5
 * 7777 = 0
 * 9999 = 4
 * 7756 = 1
 * 6855 = 3
 * 9881 = 5
 * 5531 = 0
 *
 * 2581 = ?
 * """
 *
 * Note:
 * This model yields 10 solutions, since x4 is not
 * restricted in the constraints.
 * All solutions has x assigned to the correct result.
 *
 *
 * (Problem 2)
 * The problem stated in "A puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
 * is
 * """
 * 8809 = 6
 * 7662 = 2
 * 9312 = 1
 * 8193 = 3
 * 8096 = 5
 * 7756 = 1
 * 6855 = 3
 * 9881 = 5
 *
 * 2581 = ?
 * """
 * This problem instance yields two different solutions of x,
 * one is the same (correct) as for the above problem instance,
 * and one is not.
 * This is because here x0,x1,x4 and x9 are underdefined.
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object APuzzle extends CPModel with App {

  // data
  val n = 10

  // Problem type
  var p = 1 // 1,2,3,4
  if (args.length > 0) {
    p = args(0).toInt
    if (p < 1 || p > 4) {
      println("Valid p's are 1..4. Set to p=1")
      p = 1
    }
  }

  val all = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = all

  // the unknown, i.e. 2581 = x
  val x = CPIntVar(0 to n - 1)

  //
  // constraints
  //
  var numSols = 0

  if (p == 1) {

    // Problem 1 (basic approach)
    println("Problem 1 (basic approach)")
    add(x8 + x8 + x0 + x9 == 6)
    add(x7 + x1 + x1 + x1 == 0)
    add(x2 + x1 + x7 + x2 == 0)
    add(x6 + x6 + x6 + x6 == 4)
    add(x1 + x1 + x1 + x1 == 0)
    add(x3 + x2 + x1 + x3 == 0)
    add(x7 + x6 + x6 + x2 == 2)
    add(x9 + x3 + x1 + x2 == 1)
    add(x0 + x0 + x0 + x0 == 4)
    add(x2 + x2 + x2 + x2 == 0)
    add(x3 + x3 + x3 + x3 == 0)
    add(x5 + x5 + x5 + x5 == 0)
    add(x8 + x1 + x9 + x3 == 3)
    add(x8 + x0 + x9 + x6 == 5)
    add(x7 + x7 + x7 + x7 == 0)
    add(x9 + x9 + x9 + x9 == 4)
    add(x7 + x7 + x5 + x6 == 1)
    add(x6 + x8 + x5 + x5 == 3)
    add(x9 + x8 + x8 + x1 == 5)
    add(x5 + x5 + x3 + x1 == 0)

    // The unknown
    add(x2 + x5 + x8 + x1 == x)

  } else if (p == 2) {
    // Problem 1 (alternative approach)
    println("Problem 1 (alternative approach)")
    val problem1 = Array(
      Array(8, 8, 0, 9, 6),
      Array(7, 1, 1, 1, 0),
      Array(2, 1, 7, 2, 0),
      Array(6, 6, 6, 6, 4),
      Array(1, 1, 1, 1, 0),
      Array(3, 2, 1, 3, 0),
      Array(7, 6, 6, 2, 2),
      Array(9, 3, 1, 2, 1),
      Array(0, 0, 0, 0, 4),
      Array(2, 2, 2, 2, 0),
      Array(3, 3, 3, 3, 0),
      Array(5, 5, 5, 5, 0),
      Array(8, 1, 9, 3, 3),
      Array(8, 0, 9, 6, 5),
      Array(7, 7, 7, 7, 0),
      Array(9, 9, 9, 9, 4),
      Array(7, 7, 5, 6, 1),
      Array(6, 8, 5, 5, 3),
      Array(9, 8, 8, 1, 5),
      Array(5, 5, 3, 1, 0))

    for (i <- 0 until problem1.length) {
      add(sum(for (j <- 0 until 4) yield all(problem1(i)(j))) == problem1(i)(4))
    }

    add(all(2) + all(5) + all(8) + all(1) == x);

  } else if (p == 3) {

    // Problem 2 (basic approach)
    println("Problem 2 (basic approach)")

    add(x8 + x8 + x0 + x9 == 6)
    add(x7 + x6 + x6 + x2 == 2)
    add(x9 + x3 + x1 + x2 == 1)
    add(x8 + x1 + x9 + x3 == 3)
    add(x8 + x0 + x9 + x6 == 5)
    add(x7 + x7 + x5 + x6 == 1)
    add(x6 + x8 + x5 + x5 == 3)
    add(x9 + x8 + x8 + x1 == 5)

    // The unknown
    add(x2 + x5 + x8 + x1 == x)

  } else {

    // Problem 2 (alternative approach)
    println("Problem 2 (alternative approach)")
    val problem2 = Array(
      Array(8, 8, 0, 9, 6),
      Array(7, 6, 6, 2, 2),
      Array(9, 3, 1, 2, 1),
      Array(8, 1, 9, 3, 3),
      Array(8, 0, 9, 6, 5),
      Array(7, 7, 5, 6, 1),
      Array(6, 8, 5, 5, 3),
      Array(9, 8, 8, 1, 5))

    for (i <- 0 until problem2.length) {
      add(sum(for (j <- 0 until 4) yield all(problem2(i)(j))) == problem2(i)(4))
    }
    add(all(2) + all(5) + all(8) + all(1) == x)
  }

  search { binaryStatic(all) }

  onSolution {
    println("all:" + all.mkString("") + "  x:" + x)
    numSols += 1
  }

  start()

  println("\nIt was " + numSols + " solutions.\n")
}
