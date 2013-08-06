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

 Quasigroup Completion problem in Oscar.

 See 
 Carla P. Gomes and David Shmoys:
 "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
  
 See also
 Ivars Peterson "Completing Latin Squares"
 http://www.maa.org/mathland/mathtrek_5_8_00.html
 """
 Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
 a four-by-four array so that no column or row contains the same two numbers. 
 The result is known as a Latin square.
 ...
 The so-called quasigroup completion problem concerns a table that is correctly 
 but only partially filled in. The question is whether the remaining blanks in 
 the table can be filled in to obtain a complete Latin square (or a proper 
 quasigroup multiplication table).
 """
 
 @author Hakan Kjellerstrand hakank@gmail.com
 http://www.hakank.org/oscar/
 
*/
object QuasigroupCompletion {

  def main(args: Array[String]) {

    val cp = CPSolver()

    val X = 0

    // Default problem instance 
    // Example from Gomes & Shmoys, page 7
    // Two solutions.
   var n = 4 
   var problem = List(List(X,1,X,X),
                      List(X,X,2,X),
                      List(X,3,X,X),
                      List(X,X,X,4))

  // read problem instance from file
  if (args.length > 0) {
    println("\nReading from file: " + args(0))
    
    val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
    n = lines(0).toInt
    println("n:" + n)
    problem = lines.tail.map(_.split(" ").toList.map(i=>if (i == ".") X else i.toInt))

  }
  


    // variables
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 1 to n)))

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {

      // fill the things we know
      for (i <- 0 until n; j <- 0 until n) {
        if (problem(i)(j) > X) {
          cp.add(x(i)(j) == problem(i)(j))
        }
      }
  
      // make it a Latin Square
      for (i <- 0 until n) {
        cp.add(allDifferent( Array.tabulate(n)(j=> x(i)(j)) ), Strong)
        cp.add(allDifferent( Array.tabulate(n)(j=> x(j)(i)) ), Strong)
      }


     } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
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
