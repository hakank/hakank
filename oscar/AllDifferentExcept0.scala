package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.core._

/**
 * Decomposition of the global constraint allDifferent_except_0 in in Oscar.
 *
 *  From Global constraint catalogue:
 * http://www.emn.fr/x-info/sdemasse/gccat/CallDifferent_except_0.html
 *  """
 * Enforce all variables of the collection VARIABLES to take distinct
 * values, except those variables that are assigned to 0.
 *
 * Example
 *    (<5, 0, 1, 9, 0, 3>)
 *
 * The allDifferent_except_0 constraint holds since all the values
 * (that are different from 0) 5, 1, 9 and 3 are distinct.
 * """
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 */
object AllDifferentExcept0 extends CPModel with App {

  // Data
  val n = if (args.length > 0) args(0).toInt else 7

  // Variables
  val x = Array.fill(n)(CPIntVar(0 to n))
  val occurrences = Array.tabulate(n + 1)(i => (i, CPIntVar(0 to n)))
  val z = occurrences(0)._2 // the tuple is (#occurrences, value)

  // Constraints
  
  // Decomposition of allDifferent_except_0
  for (i <- 0 until x.size; j <- 0 until i) {
    add(((x(i) !== 0) && (x(j) !== 0)) ==> (x(i) !== x(j)))
  }

  // Just for fun, we add that x should be increasing
  
  // Decomposition of increasing
  for (i <- 1 until x.size) {
    add(x(i - 1) <= x(i)) 
  }

  // and that there must be exactly 2 0's
  add(gcc(x, occurrences))
  add(z == 2)

  search { binaryFirstFail(x) }

  onSolution {
    println("x:" + x.mkString(""))
    println("z:" + z)
    println()
  }

  val stats = start()
  println(stats)
}
