/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Strimko problem in Oscar.

  From
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain
  the number 1, 2, ... n exactly once (that is, the grid must form a
  Latin square), and each "stream" (connected path in the grid) must
  also contain the numbers 1, 2, ..., n exactly once.
  """

  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm

  I blogged about this problem (using MiniZinc model) in
  'Strimko - Latin squares puzzle with "streams"'
  http://www.hakank.org/constraint_programming_blog/2009/08/strimko_latin_squares_puzzle_w_1.html


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Strimko {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val streams = Array(Array(1,1,2,2,2,2,2),
                        Array(1,1,2,3,3,3,2),
                        Array(1,4,1,3,3,5,5),
                        Array(4,4,3,1,3,5,5),
                        Array(4,6,6,6,7,7,5),
                        Array(6,4,6,4,5,5,7),
                        Array(6,6,4,7,7,7,7))

    // Note: This is 1-based (fixed below)
    val placed = Array(Array(2,1,1),
                       Array(2,3,7),
                       Array(2,5,6),
                       Array(2,7,4),
                       Array(3,2,7),
                       Array(3,6,1),
                       Array(4,1,4),
                       Array(4,7,5),
                       Array(5,2,2),
                       Array(5,6,6))

    val n = streams.length
    val num_placed = placed.length


    //
    // variables
    //
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 1 to n)))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // all rows and columns must be unique, i.e. a Latin Square
      for(i <- 0 until n) {
        // rows
        cp.add(allDifferent(for(j <- 0 until n) yield x(i)(j)), Strong)
        // cols
        cp.add(allDifferent(for(j <- 0 until n) yield x(j)(i)), Strong)
      }
      
      // Handle the streams
      for(s <- 1 to n) {
        cp.add(allDifferent(for{i <- 0 until n
                                j <- 0 until n if streams(i)(j) == s} yield x(i)(j)), Strong)

        
      }
      
      // Handle the placed
      for(i <- 0 until num_placed) {
        // note: also adjust to 0-based
        cp.add(x(placed(i)(0) - 1)(placed(i)(1)- 1) ==  placed(i)(2))
      }


    } exploration {
       
      cp.binaryFirstFail(x.flatten)

      for(i <- 0 until n) {
        println(x(i).mkString(""))
      }
      println()

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
