/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Sicherman Dice in Oscar.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  ""
  Sicherman dice are the only pair of 6-sided dice which are not normal dice,
  bear only positive integers, and have the same probability distribution for
  the sum as normal dice.

  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  ""

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:

   x1 = [1, 2, 3, 4, 5, 6]
   x2 = [1, 2, 3, 4, 5, 6]
   ----------
   x1 = [1, 2, 2, 3, 3, 4]
   x2 = [1, 3, 4, 5, 6, 8]
   *

  Extra: If we also allow 0 (zero) as a valid value then the
  following two solutions are also valid:

  x1 = [0, 1, 1, 2, 2, 3]
  x2 = [2, 4, 5, 6, 7, 9]
  ----------
  x1 = [0, 1, 2, 3, 4, 5]
  x2 = [2, 3, 4, 5, 6, 7]

  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html



  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SichermanDice {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6
    val m = 10
    val lowest_value = 1

    val RANGE = 0 until n
    val RANGE1 = 0 until n-1

    // standard distribution
    val standard_dist = Array(1,2,3,4,5,6,5,4,3,2,1)


    //
    // variables
    //
    val x1 = Array.fill(n)(CPVarInt(cp, lowest_value to m))
    val x2 = Array.fill(n)(CPVarInt(cp, lowest_value to m))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(k <- 0 until standard_dist.length) {
        cp.add(
               sum(for{i <- RANGE
                       j <- RANGE} yield x1(i) + x2(j) === (k + 2)) 
               == standard_dist(k))
      }
      
      // symmetry breaking
      for(i <- RANGE1) {
        cp.add(x1(i) <= x1(i+1)) // increasing x1
        cp.add(x2(i) <= x2(i+1)) // increasing x2
        cp.add(x1(i) <= x2(i))   // x1 <= x2
      }
      

    } exploration {
       
      cp.binary(x1 ++ x2, _.min, _.min)

      println("x1: " + x1.mkString(""))
      println("x2: " + x2.mkString(""))
      println()

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
