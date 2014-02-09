package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Added corner puzzle in Oscar.

  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the circles and 
  squares (one digit in each figure) so that the number in each square is equal 
  to the sum on the numbers in the circles which  adjoin it.  
  ...
  
     C F C
     F   F
     C F C
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object AddedCorner extends CPModel with App {

  // Data
  val n = 8

  // Variables
  val x = Array.fill(n)(CPIntVar(1 to n))
  val Array(a, b, c, d, e, f, g, h) = x

  // Constraints
  add(allDifferent(x), Strong)
  add(b == a + c)
  add(d == a + f)
  add(e == c + h)
  add(g == f + h)

  search { binaryFirstFail(x) }

  var nSols = 0
  onSolution {
    println(a + " " + b + " " + c)
    println(d + "   " + " " + e)
    println(f + " " + g + " " + h)
    println()
    nSols += 1
  }

  val stats = start()
  println(stats)
}
