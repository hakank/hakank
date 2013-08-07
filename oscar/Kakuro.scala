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

  Kakuro puzzle in Oscar.

  http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive
  into each white cell such that the sum of the numbers in each entry
  matches the clue associated with it and that no digit is duplicated in
  any entry. It is that lack of duplication that makes creating Kakuro
  puzzles with unique solutions possible, and which means solving a Kakuro
  puzzle involves investigating combinations more, compared to Sudoku in
  which the focus is on permutations. There is an unwritten rule for
  making Kakuro puzzles that each clue must have at least two numbers
  that add up to it. This is because including one number is mathematically
  trivial when solving Kakuro puzzles; one can simply disregard the
  number entirely and subtract it from the clue it indicates.
  """
  
  This model solves the problem at the Wikipedia page.
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg
  
  The solution:
   9 7 0 0 8 7 9
   8 9 0 8 9 5 7
   6 8 5 9 7 0 0
   0 6 1 0 2 6 0
   0 0 4 6 1 3 2
   8 9 3 1 0 1 4
   3 1 2 0 0 2 1


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Kakuro {


  /**
   * Ensure that the sum of the segments
   * in cc == res
   *
   */
  def calc(cp: CPSolver,
           cc: Array[Int],
           x: Array[Array[CPVarInt]],
           res: Int) {

    // ensure that the values are positive
    val len = (cc.length / 2).toInt
    for(i <- 0 until len) {
      cp.add(x(cc(i*2)-1)(cc(i*2+1)-1) >= 1)
    }

    // sum the numbers
    cp.add(sum(for{i <- 0 until len} yield x(cc(i*2)-1)(cc(i*2+1)-1)) == res)
  }

  
  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    //
    // variables
    //
    // size of matrix
    val n = 7

    // segments:
    //  sum, the segments
    // Note: this is 1-based
    val problem = Array(
                        Array(16,  1,1, 1,2),
                        Array(24,  1,5, 1,6, 1,7),
                        Array(17,  2,1, 2,2),
                        Array(29,  2,4, 2,5, 2,6, 2,7),
                        Array(35,  3,1, 3,2, 3,3, 3,4, 3,5),
                        Array( 7,  4,2, 4,3),
                        Array( 8,  4,5, 4,6),
                        Array(16,  5,3, 5,4, 5,5, 5,6, 5,7),
                        Array(21,  6,1, 6,2, 6,3, 6,4),
                        Array( 5,  6,6, 6,7),
                        Array( 6,  7,1, 7,2, 7,3),
                        Array( 3,  7,6, 7,7),
                        
                        Array(23,  1,1, 2,1, 3,1),
                        Array(30,  1,2, 2,2, 3,2, 4,2),
                        Array(27,  1,5, 2,5, 3,5, 4,5, 5,5),
                        Array(12,  1,6, 2,6),
                        Array(16,  1,7, 2,7),
                        Array(17,  2,4, 3,4),
                        Array(15,  3,3, 4,3, 5,3, 6,3, 7,3),
                        Array(12,  4,6, 5,6, 6,6, 7,6),
                        Array( 7,  5,4, 6,4),
                        Array( 7,  5,7, 6,7, 7,7),
                        Array(11,  6,1, 7,1),
                        Array(10,  6,2, 7,2))


    val num_p = problem.length // Number of segments

    // The blanks
    // Note: 1-based
    val blanks = Array(
                       Array(1,3), Array(1,4),
                       Array(2,3),
                       Array(3,6), Array(3,7),
                       Array(4,1), Array(4,4), Array(4,7),
                       Array(5,1), Array(5,2),
                       Array(6,5),
                       Array(7,4), Array(7,5))

    val num_blanks = blanks.length


    //
    // Variables
    // 
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 0 to 9)))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // fill the blanks with 0
      for(i <- 0 until num_blanks) {
        cp.add(x(blanks(i)(0)-1)(blanks(i)(1)-1) == 0)
      }

      for(i <- 0 until num_p) {
        val segment = problem(i)

        // Remove the sum from the segment
        val s2 = for(i<-1 until segment.length) yield segment(i)                                                  
        // sum this segment
        calc(cp, s2, x, segment(0))

        // all numbers in this segment must be distinct
        val len = segment.length / 2
        cp.add( allDifferent(for(j <- 0 until len) yield x(s2(j * 2) - 1)(s2(j * 2 + 1) - 1)))

      }

    } exploration {
       
      cp.binaryMaxDegree(x.flatten)

      for(i <- 0 until n) {
        for(j <- 0 until n) {
          val v = x(i)(j).value
          if (v > 0) {
            print(v + " ")
          } else {
            print("  ")
          }
        }
        println()
      }
      println()

      numSols += 1

   } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
