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
 * Sudoku solver in Oscar.
 *
 * See http://en.wikipedia.org/wiki/Sudoku
 *
 * This version can also read a problem instance from a file.
 *
 * A collection of problem instances (from Gecode) can be found 
 * in the ./data directory.
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Sudoku2 {

  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var n = 9
    var reg = 3
    val X = 0

    // 
    // data
    //
    // This problem is problem 0 from
    // Gecode's sudoku.cpp
    // http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
    //
    var problem = Array(Array(X, X, X, 2, X, 5, X, X, X),
                        Array(X, 9, X, X, X, X, 7, 3, X),
                        Array(X, X, 2, X, X, 9, X, 6, X),
                        Array(2, X, X, X, X, X, 4, X, 9),
                        Array(X, X, X, X, 7, X, X, X, X),
                        Array(6, X, 9, X, X, X, X, X, 1),
                        Array(X, 8, X, 4, X, X, 1, X, X),
                        Array(X, 6, 3, X, X, X, X, 8, X),
                        Array(X, X, X, 6, X, 8, X, X, X))

    val alpha = ('0' to'9') ++ ('A' to 'Z')

   // read problem instance from file
   if (args.length > 0) {
     val fileName = args(0)
     println("\nReading from file: " + fileName)
    
     val lines = fromFile(fileName).getLines.filter(!_.startsWith("#")).toArray
     n = lines(0).toInt
     reg = sqrt(n).toInt
     println("Size:" + n)
     val this_problem = lines.tail.map(_.split("\\s+").
                        filter(_.length>0).map(i=>if (i == ".") X else i.toInt))
     println(this_problem.map(row=>row.
                              map(e=>if (e==0) "  ." else "%3s".format(alpha(e))).mkString("")).
             mkString("\n"))

     println()
    
     problem = this_problem

    }

    val NRANGE = 0 until n
    val RRANGE = 0 until reg

    // variables
    val x = Array.fill(n,n)(CPIntVar(1 to n)(cp))
    val x_t = x.transpose

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {
      // fill with the hints
      for(i <- NRANGE;
          j <- NRANGE if problem(i)(j) > 0) {
            cp.add(x(i)(j) == problem(i)(j))
      }

     // rows and columns
     for(i <- NRANGE) {
       cp.add(allDifferent(x(i)), Strong)
       cp.add(allDifferent(x_t(i)), Strong)
     }

     // blocks
     for(i <- RRANGE; j <- RRANGE) {
       cp.add(allDifferent((for{ r <- i*reg until i*reg+reg;
                                 c <- j*reg until j*reg+reg
             } yield x(r)(c))), Strong)
     }


     } search {
       
       binaryFirstFail(x.flatten.toSeq)
       
     } onSolution {
       
       println("\nSolution:")
       for(i <- NRANGE) {
         println(x(i).map(j=>alpha(j.value)).mkString(" "))
       }
       println()

       numSols += 1
       
     }
     
     println(cp.start())

   }

}
