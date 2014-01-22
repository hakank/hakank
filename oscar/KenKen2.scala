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

/**
 *
 * KenKen puzzle in OscaR.
 *
 * http://en.wikipedia.org/wiki/KenKen
 * """
 * KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
 * several characteristics with sudoku. The name comes from Japanese and
 * is translated as 'square wisdom' or 'cleverness squared'.
 * ...
 * The objective is to fill the grid in with the digits 1 through 6 such that:
 *
 * - Each row contains exactly one of each digit
 * - Each column contains exactly one of each digit
 * - Each bold-outlined group of cells is a cage containing digits which
 *   achieve the specified result using the specified mathematical operation:
 *     addition (+),
 *     subtraction (-),
 *     multiplication (x),
 *     and division (/).
 *    (Unlike in Killer sudoku, digits may repeat within a group.)
 *
 * ...
 * More complex KenKen problems are formed using the principles described
 * above but omitting the symbols +, -, x and /, thus leaving them as
 * yet another unknown to be determined.
 * """
 *
 * The solution is:
 *
 *    5 6 3 4 1 2
 *    6 1 4 5 2 3
 *    4 5 2 3 6 1
 *    3 4 1 2 5 6
 *    2 3 6 1 4 5
 *    1 2 5 6 3 4
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */

object KenKen2 {


  /**
   * Ensure that the sum of the segments
   * in cc == res
   *
   */
  def calc(cp: CPSolver,
           cc: Array[Int],
           x: Array[Array[CPIntVar]],
           res: Int) {

    val ccLen = cc.length
    if (ccLen == 4) {

      // for two operands there's
      // a lot of possible variants
      val a = x(cc(0)-1)(cc(1)-1)
      val b = x(cc(2)-1)(cc(3)-1)

      val r1 = a + b === res
      val r2 = a * b === res
      val r3 = a * res === b
      val r4 = b * res === a
      val r5 = a - b === res
      val r6 = b - a === res

      cp.add(r1+r2+r3+r4+r5+r6 >= 1)

    } else {

      // For length > 2 then res is either the sum
      // or the product of the segment

      // sum the numbers
      val len = ccLen / 2
        val xx = for{i <- 0 until len} yield x(cc(i*2)-1)(cc(i*2+1)-1)

      // Sum
      val this_sum = sum(xx) === res

      // Product
      var this_prod = CPIntVar(0 to 9)(cp)
      if (xx.length == 3) {
        this_prod = (x(cc(0)-1)(cc(1)-1) *
                     x(cc(2)-1)(cc(3)-1) *
                     x(cc(4)-1)(cc(5)-1)) === res
      } else {
        this_prod = (
                     x(cc(0)-1)(cc(1)-1) *
                     x(cc(2)-1)(cc(3)-1) *
                     x(cc(4)-1)(cc(5)-1) *
                     x(cc(6)-1)(cc(7)-1)) === res

      }

      cp.add(this_sum + this_prod >= 1)

    }

  }
  
  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6
    val RANGE = 0 until n

    // For a better view of the problem, see
    //  http://en.wikipedia.org/wiki/File:KenKenProblem.svg

    // hints
    //  sum, the hints
    // Note: this is 1-based (fixed in calc())
    val problem = Array(Array( 11,  1,1, 2,1),
                        Array(  2,  1,2, 1,3),
                        Array( 20,  1,4, 2,4),
                        Array(  6,  1,5, 1,6, 2,6, 3,6),
                        Array(  3,  2,2, 2,3),
                        Array(  3,  2,5, 3,5),
                        Array(240,  3,1, 3,2, 4,1, 4,2),
                        Array(  6,  3,3, 3,4),
                        Array(  6,  4,3, 5,3),
                        Array(  7,  4,4, 5,4, 5,5),
                        Array( 30,  4,5, 4,6),
                        Array(  6,  5,1, 5,2),
                        Array(  9,  5,6, 6,6),
                        Array(  8,  6,1, 6,2, 6,3),
                        Array(  2,  6,4, 6,5))


    val num_p = problem.length // Number of segments

    //
    // Decision variables
    // 
    val x = Array.fill(n,n)(CPIntVar(1 to n)(cp))
    val x_flat = x.flatten

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // rows and columns
      for(i <- RANGE) {
        cp.add(allDifferent( Array.tabulate(n)(j=> x(i)(j))), Strong)
        cp.add(allDifferent( Array.tabulate(n)(j=> x(j)(i))), Strong)
      }
      
      for(i <- 0 until num_p) {
        val segment = problem(i)

        // Remove the sum from the segment
        val s2 = for(i<-1 until segment.length) yield segment(i)                                                  
        // calculate this segment
        calc(cp, s2, x, segment(0))


      }

    } search {
       
      binaryMaxDegree(x_flat)

    } onSolution {
      
      for(i <- RANGE) {
        println(x(i).mkString(""))
      }
      println()

      numSols += 1

   } 
   println(cp.start())  
  }

}
