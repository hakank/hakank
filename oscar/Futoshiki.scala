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

  Futoshiki puzzle in Oscar.

  From http://en.wikipedia.org/wiki/Futoshiki
  """
  The puzzle is played on a square grid, such as 5 x 5. The objective
  is to place the numbers 1 to 5 (or whatever the dimensions are)
  such that each row, and column contains each of the digits 1 to 5.
  Some digits may be given at the start. In addition, inequality
  constraints are also initially specifed between some of the squares,
  such that one must be higher or lower than its neighbour. These
  constraints must be honoured as the grid is filled out.
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Futoshiki {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
   //
    // Example from Tailor model futoshiki.param/futoshiki.param
    // Solution:
    // 5 1 3 2 4
    // 1 4 2 5 3
    // 2 3 1 4 5
    // 3 5 4 1 2
    // 4 2 5 3 1
    //
    // Futoshiki instance, by Andras Salamon
    //
    val values1 = Array(
                        Array(0, 0, 3, 2, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0))
    

    // [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
    // Note: 1-based (changed to 0-based below)
    val lt1 = Array(Array(1,2,  1,1),
                    Array(1,4,  1,5),
                    Array(2,3,  1,3),
                    Array(3,3,  2,3),
                    Array(3,4,  2,4),
                    Array(2,5,  3,5),
                    Array(3,2,  4,2),
                    Array(4,4,  4,3),
                    Array(5,2,  5,1),
                    Array(5,4,  5,3),
                    Array(5,5,  4,5))


    //
    // Example from http://en.wikipedia.org/wiki/Futoshiki
    //
    // Solution:
    // 5 4 3 2 1
    // 4 3 1 5 2
    // 2 1 4 3 5
    // 3 5 2 1 4
    // 1 2 5 4 3
    //
    val values2 = Array(Array(0, 0, 0, 0, 0),
                        Array(4, 0, 0, 0, 2),
                        Array(0, 0, 4, 0, 0),
                        Array(0, 0, 0, 0, 4),
                        Array(0, 0, 0, 0, 0))

    // Note: 1-based
    val lt2 = Array(Array(1,2,  1,1),
                    Array(1,4,  1,3),
                    Array(1,5,  1,4),
                    Array(4,4,  4,5),
                    Array(5,1,  5,2),
                    Array(5,2,  5,3))


    var problem = 1
    var values = values1
    var lt     = lt1

    if (args.length > 0) {
      problem = args(0).toInt
      if (problem < 1 || problem > 2) {
        problem = 1
      }
    }

    if (problem == 2) {
      values = values2
      lt     = lt2
    }

    println("Solving problem" + problem)

    val size = values.length
    val RANGE = 0 until size
    val NUMQD = 0 until lt.length


    //
    // variables
    //
    val field = Array.fill(size)(Array.fill(size)(CPIntVar(1 to size)(cp)))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // set initial values
      for(row <- RANGE; col <- RANGE if values(row)(col) > 0) {
        cp.add(field(row)(col) == values(row)(col))
      }

      // all rows have to be different
      for(row <- RANGE) {
        cp.add(allDifferent(for(col <- RANGE) yield field(row)(col)))
      }
      
      // all columns have to be different
      for(col <- RANGE) {
        cp.add(allDifferent(for(row <- RANGE) yield field(row)(col)))
      }

      // all < constraints are satisfied
      // Also: make 0-based
      for(i <- NUMQD) {
        cp.add(field(lt(i)(0)-1)(lt(i)(1)-1) <
               field(lt(i)(2)-1)(lt(i)(3)-1 ))
      }


    } search {
      binaryFirstFail(field.flatten.toSeq)
    } onSolution {
      for(i <- RANGE) {
        println(field(i).mkString(""))
      }
      println()

      numSols += 1

   } 

    println(cp.start())


  }

}
