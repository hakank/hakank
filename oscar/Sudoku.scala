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
import scala.math.pow

/**
 *
 * Sudoku solver in Oscar.
 * 
 * See http://en.wikipedia.org/wiki/Sudoku
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Sudoku {

  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 9
    val reg = 3

    val NRANGE = 0 until n
    val RRANGE = 0 until reg

    // 
    // data
    //
    // This problem is problem 0 from
    // Gecode's sudoku.cpp
    // http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
    //
    val problem = Array(Array(0, 0, 0, 2, 0, 5, 0, 0, 0),
                        Array(0, 9, 0, 0, 0, 0, 7, 3, 0),
                        Array(0, 0, 2, 0, 0, 9, 0, 6, 0),
                        Array(2, 0, 0, 0, 0, 0, 4, 0, 9),
                        Array(0, 0, 0, 0, 7, 0, 0, 0, 0),
                        Array(6, 0, 9, 0, 0, 0, 0, 0, 1),
                        Array(0, 8, 0, 4, 0, 0, 1, 0, 0),
                        Array(0, 6, 3, 0, 0, 0, 0, 8, 0),
                        Array(0, 0, 0, 6, 0, 8, 0, 0, 0))

    // variables
    val x = Array.fill(n,n)(CPVarInt(cp, 1 to n))
    val x_t = x.transpose

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // fill with the hints
      NRANGE.foreach(i=>NRANGE.foreach(j=>if (problem(i)(j) > 0) cp.add(x(i)(j) == problem(i)(j))))

      // rows and columns
      NRANGE.foreach(i=>cp.add(allDifferent(x(i)), Strong))
      NRANGE.foreach(j=>cp.add(allDifferent(x_t(j)), Strong))
      

      // blocks
      for(i <- RRANGE; 
          j <- RRANGE) {
        cp.add(allDifferent((for{ r <- i*reg until i*reg+reg;
                                  c <- j*reg until j*reg+reg
              } yield x(r)(c))), Strong)
      }


    } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
       for(i <- 0 until n) {
         println(x(i).mkString(" "))
       }
       println()

       numSols += 1
       
    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
