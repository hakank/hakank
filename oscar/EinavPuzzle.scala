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

  A programming puzzle from Einav in Oscar.

  From
  "A programming puzzle from Einav"
  http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
  """
  My friend Einav gave me this programming puzzle to work on. Given
  this array of positive and negative numbers:
    33   30  -10 -6  18   7  -11 -23   6
    ...
    -25   4  16  30  33 -23  -4   4 -23

  You can flip the sign of entire rows and columns, as many of them
  as you like. The goal is to make all the rows and columns sum to positive
  numbers (or zero), and then to find the solution (there are more than one)
  that has the smallest overall sum. So for example, for this array:
    33  30 -10
    -16  19   9
    -17 -12 -14
  You could flip the sign for the bottom row to get this array:
    33  30 -10
    -16  19   9
    17  12  14
  Now all the rows and columns have positive sums, and the overall total is
  108.
  But you could instead flip the second and third columns, and the second
  row, to get this array:
    33  -30  10
    16   19    9
    -17   12   14
  All the rows and columns still total positive, and the overall sum is just
  66. So this solution is better (I don't know if it's the best)
  A pure brute force solution would have to try over 30 billion solutions.
  I wrote code to solve this in J. I'll post that separately.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object EinavPuzzle {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // Small problem
    // val rows = 3
    // val cols = 3
    // val data = Array(Array( 33,  30, -10),
    //                  Array(-16,  19,   9),
    //                  Array(-17, -12, -14))

    // Full problem
    val rows = 27
    val cols = 9
    val data = Array(
                     Array(33,30,10,-6,18,-7,-11,23,-6),
                     Array(16,-19,9,-26,-8,-19,-8,-21,-14),
                     Array(17,12,-14,31,-30,13,-13,19,16),
                     Array(-6,-11,1,17,-12,-4,-7,14,-21),
                     Array(18,-31,34,-22,17,-19,20,24,6),
                     Array(33,-18,17,-15,31,-5,3,27,-3),
                     Array(-18,-20,-18,31,6,4,-2,-12,24),
                     Array(27,14,4,-29,-3,5,-29,8,-12),
                     Array(-15,-7,-23,23,-9,-8,6,8,-12),
                     Array(33,-23,-19,-4,-8,-7,11,-12,31),
                     Array(-20,19,-15,-30,11,32,7,14,-5),
                     Array(-23,18,-32,-2,-31,-7,8,24,16),
                     Array(32,-4,-10,-14,-6,-1,0,23,23),
                     Array(25,0,-23,22,12,28,-27,15,4),
                     Array(-30,-13,-16,-3,-3,-32,-3,27,-31),
                     Array(22,1,26,4,-2,-13,26,17,14),
                     Array(-9,-18,3,-20,-27,-32,-11,27,13),
                     Array(-17,33,-7,19,-32,13,-31,-2,-24),
                     Array(-31,27,-31,-29,15,2,29,-15,33),
                     Array(-18,-23,15,28,0,30,-4,12,-32),
                     Array(-3,34,27,-25,-18,26,1,34,26),
                     Array(-21,-31,-10,-13,-30,-17,-12,-26,31),
                     Array(23,-31,-19,21,-17,-10,2,-23,23),
                     Array(-3,6,0,-3,-32,0,-10,-25,14),
                     Array(-19,9,14,-27,20,15,-5,-27,18),
                     Array(11,-6,24,7,-17,26,20,-31,-25),
                     Array(-25,4,-16,30,33,23,-4,-4,23))


    val ROWS = 0 until rows
    val COLS = 0 until cols

    val sign_domains = Set(-1,1)

    //
    // variables
    //
    val x = Array.fill(rows,cols)(CPVarInt(cp, -100 to 100))
    val total_sum = sum(x.flatten)
    
    val row_signs = Array.fill(rows)(CPVarInt(cp, sign_domains))
    val col_signs = Array.fill(cols)(CPVarInt(cp, sign_domains))

    val row_sums = for(i <- ROWS) yield sum(for(j <- COLS) yield x(i)(j))
    val col_sums = for(j <- COLS) yield sum(for(i <- ROWS) yield x(i)(j))
      
    //
    // constraints
    //
    var numSols = 0

    cp.minimize(total_sum) subjectTo {

      for(i <- ROWS; j <- COLS) {
        cp.add(x(i)(j) == row_signs(i)*col_signs(j) * data(i)(j))
      }
      
      for(i <- ROWS) {
        cp.add(row_sums(i) >= 0)
      }
      
      for(j <- COLS) {
        cp.add(col_sums(j) >= 0)
      }


    } search {
       
      binaryMaxDegree(col_signs ++ row_signs)
    } onSolution {
      
      println("\nSolution:")
      println("row_sums: " + row_sums.mkString(""))
      println("row_signs: " + row_signs.mkString(""))
      println("col_sums: " + col_sums.mkString(""))
      println("col_signs: " + col_signs.mkString(""))
      
      for(i <- ROWS) {
        for(j <- COLS) {
          print("%4d".format(x(i)(j).value))
        }
        println()
      }
      println()

    } 
    
    println(cp.start())

  }

}
