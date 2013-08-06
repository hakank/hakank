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

/**
 *
 * http://en.wikipedia.org/wiki/Killer_Sudoku
 * """
 * Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or
 * samunamupure) is a puzzle that combines elements of sudoku and kakuro.
 * Despite the name, the simpler killer sudokus can be easier to solve
 * than regular sudokus, depending on the solver's skill at mental arithmetic;
 * the hardest ones, however, can take hours to crack.
 *
 * ...
 *
 * The objective is to fill the grid with numbers from 1 to 9 in a way that
 * the following conditions are met:
 *
 * - Each row, column, and nonet contains each number exactly once.
 * - The sum of all numbers in a cage must match the small number printed
 *   in its corner.
 * - No number appears more than once in a cage. (This is the standard rule
 *   for killer sudokus, and implies that no cage can include more
 *   than 9 cells.)
 *
 * In 'Killer X', an additional rule is that each of the long diagonals
 * contains each number once.
 * """
 *
 * Here we solve the problem from the Wikipedia page, also shown here
 * http://en.wikipedia.org/wiki/File:Killersudoku_color.svg
 *
 * The output is:
 *   2 1 5 6 4 7 3 9 8
 *   3 6 8 9 5 2 1 7 4
 *   7 9 4 3 8 1 6 5 2
 *   5 8 6 2 7 4 9 3 1
 *   1 4 2 5 9 3 8 6 7
 *   9 7 3 8 1 6 4 2 5
 *   8 2 1 7 3 9 5 4 6
 *   6 5 9 4 2 8 7 1 3
 *   4 3 7 1 6 5 2 8 9
 *
 * 
 *  @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */

object KillerSudoku {


  /**
   * Ensure that the sum of the segments
   * in cc == res
   *
   */
  def calc(cp: CPSolver,
           cc: Array[Int],
           x: Array[Array[CPVarInt]],
           res: Int) {

    val len = (cc.length / 2).toInt

    // sum the numbers
    cp.add(sum(for{i <- 0 until len} yield x(cc(i*2)-1)(cc(i*2+1)-1)) == res)
  }
  
  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // size of matrix
    val cell_size = 3
    val CELLS = 0 until cell_size
    val n = cell_size*cell_size
    val RANGE = 0 until n

    // For a better view of the problem, see
    //  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

    // hints
    //  sum, the hints
    // Note: this is 1-based
    val problem = Array(Array( 3,  1,1,  1,2),
                        Array(15,  1,3,  1,4, 1,5),
                        Array(22,  1,6,  2,5, 2,6, 3,5),
                        Array(4,   1,7,  2,7),
                        Array(16,  1,8,  2,8),
                        Array(15,  1,9,  2,9, 3,9, 4,9),
                        Array(25,  2,1,  2,2, 3,1, 3,2),
                        Array(17,  2,3,  2,4),
                        Array( 9,  3,3,  3,4, 4,4),
                        Array( 8,  3,6,  4,6, 5,6),
                        Array(20,  3,7,  3,8, 4,7),
                        Array( 6,  4,1,  5,1),
                        Array(14,  4,2,  4,3),
                        Array(17,  4,5,  5,5, 6,5),
                        Array(17,  4,8,  5,7, 5,8),
                        Array(13,  5,2,  5,3, 6,2),
                        Array(20,  5,4,  6,4, 7,4),
                        Array(12,  5,9,  6,9),
                        Array(27,  6,1,  7,1, 8,1, 9,1),
                        Array( 6,  6,3,  7,2, 7,3),
                        Array(20,  6,6,  7,6, 7,7),
                        Array( 6,  6,7,  6,8),
                        Array(10,  7,5,  8,4, 8,5, 9,4),
                        Array(14,  7,8,  7,9, 8,8, 8,9),
                        Array( 8,  8,2,  9,2),
                        Array(16,  8,3,  9,3),
                        Array(15,  8,6,  8,7),
                        Array(13,  9,5,  9,6, 9,7),
                        Array(17,  9,8,  9,9))


    val num_p = problem.length // Number of segments
    println("num_p: " + num_p)

    //
    // Decision variables
    // 
    val x = Array.fill(n,n)(CPVarInt(cp, 0 to 9))
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
      
      // blocks
      for(i <- CELLS; j <- CELLS) {
        cp.add(allDifferent(  (for{ r <- i*cell_size until i*cell_size+cell_size;
                                    c <- j*cell_size until j*cell_size+cell_size
              } yield x(r)(c)).toArray), Strong)
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
       
      cp.binaryFirstFail(x_flat)

      for(i <- RANGE) {
        println(x(i).mkString(""))
      }
      println()

      numSols += 1

   } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
