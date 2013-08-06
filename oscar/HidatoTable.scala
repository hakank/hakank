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
import Array._


/*

  Hidato puzzle in Oscar.

  This version use the table constraint and is
  based on the Google or-tools (Python/C#) models
  (originally by Laurent Perron):
    http://www.hakank.org/or-tools/hidato_table.py
    http://www.hakank.org/or-tools/hidato_table.cs

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object HidatoTable {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // solution:
    //  6 7 9
    //  5 2 8
    //  1 4 3
    val problem1 = Array(Array(6,0,9),
                         Array(0,2,8),
                         Array(1,0,0))
      
    // solution :
    // 
    // 45 44 41 40 39 30 31
    // 46 43 42 28 29 38 32
    // 47 1 3 26 27 33 37
    // 48 2 25 4 34 35 36
    // 49 16 24 23 5 6 8
    // 17 19 15 22 12 7 9
    // 18 20 21 14 13 11 10
    val problem2 = Array(Array( 0,44,41, 0, 0, 0, 0),
                         Array( 0,43, 0,28,29, 0, 0),
                         Array( 0, 1, 0, 0, 0,33, 0),
                         Array( 0, 2,25, 4,34, 0,36),
                         Array(49,16, 0,23, 0, 0, 0),
                         Array( 0,19, 0, 0,12, 7, 0),
                         Array( 0, 0, 0,14, 0, 0, 0))



    // Some problem from the book:
    // Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

    // Problem 15 (Intermediate)
    val problem3 = Array(Array(64, 0, 0, 0, 0, 0, 0, 0),
                         Array( 1,63, 0,59,15,57,53, 0),
                         Array( 0, 4, 0,14, 0, 0, 0, 0),
                         Array( 3, 0,11, 0,20,19, 0,50),
                         Array( 0, 0, 0, 0,22, 0,48,40),
                         Array( 9, 0, 0,32,23, 0, 0,41),
                         Array(27, 0, 0, 0,36, 0,46, 0),
                         Array(28,30, 0,35, 0, 0, 0, 0))


    // Problem 156 (Master)
    val problem4 = Array(Array(88, 0, 0,100, 0, 0,37,0, 0,34),
                         Array( 0,86, 0,96,41, 0, 0,36, 0, 0),
                         Array( 0,93,95,83, 0, 0, 0,31,47, 0),
                         Array( 0,91, 0, 0, 0, 0, 0,29, 0, 0),
                         Array(11, 0, 0, 0, 0, 0, 0,45,51, 0),
                         Array( 0, 9, 5, 3, 1, 0, 0, 0, 0, 0),
                         Array( 0,13, 4, 0, 0, 0, 0, 0, 0, 0),
                         Array(15, 0, 0,25, 0, 0,54,67, 0, 0),
                         Array( 0,17, 0,23, 0,60,59, 0,69, 0),
                         Array(19,0,21,62,63, 0, 0, 0, 0, 0))
      

    // Problem 188 (Genius)
    val problem5 = Array(Array(  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0),
                         Array(136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0),
                         Array(139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0),
                         Array(  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103),
                         Array(  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101),
                         Array(  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0),
                         Array( 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0),
                         Array( 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92),
                         Array(  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0),
                         Array(  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0),
                         Array( 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0),
                         Array( 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88))

    val problems = Array(problem1, problem2, problem3, problem4, problem5)

    val p = if (args.length > 0) args(0).toInt else problems.length-1;

    val problem = problems(p)  
    val n = problem.length

    //
    // build the table of all valid tuples
    // (as an Array[Array[Int]])
    //
    val valid = 
      (for{i <- 0 until n; 
           j <- 0 until n; 
           a <- -1 to 1; 
           b <- -1 to 1 
           if (i+a >= 0 &&
               i+a < n &&
               j+b >= 0 &&
               j+b < n &&
               // (a != 0 || b != 0)
               (abs(a)+abs(b) > 0) // alternative
               )
             } yield Array(i*n+j, (i+a)*n + (j+b))
        ).toArray
      
    //
    // variables
    //
    val positions = Array.fill(n*n)(CPVarInt(cp, 0 to n*n-1))


    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {
      
      // fill the given hints
      for(i <- 0 until n; 
          j <- 0 until n 
          if problem(i)(j) > 0) {
            cp.add(positions(problem(i)(j)-1) == i*n+j)
      }

      cp.add(allDifferent(positions), Strong)

      for(k <- 1 until n*n-1) {
        cp.add(table(Array(positions(k), positions(k+1)), valid))
      }


    } exploration {
       
      cp.binaryFirstFail(positions)

      println("\nSolution:")

      //
      // Now, get the values of the matrix
      //
      var x = Array.tabulate(n)(i=> Array.tabulate(n)(j=> 0))
      for(k <- 0 until n*n) {
        val pos = positions(k).value
        x(pos / n)(pos % n) = k+1
      }
      
      for(i <- 0 until n) {
        for(j <- 0 until n) {
          print("%4d".format(x(i)(j)))
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
