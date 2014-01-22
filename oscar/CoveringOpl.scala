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

  Set covering problem in Oscar.

  Problem from OPL.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object CoveringOpl {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num_workers = 32
    val num_tasks = 15

    // Which worker is qualified for each task.
    // Note: This is 1-based and will be made 0-based below.
    val qualified =  Array(Array( 1,  9, 19,  22,  25,  28,  31 ),
                           Array( 2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 ),
                           Array( 3, 10, 19, 24, 26, 30, 32 ),
                           Array( 4, 21, 25, 28, 32 ),
                           Array( 5, 11, 16, 22, 23, 27, 31 ),
                           Array( 6, 20, 24, 26, 30, 32 ),
                           Array( 7, 12, 17, 25, 30, 31 ) ,
                           Array( 8, 17, 20, 22, 23  ),
                           Array( 9, 13, 14,  26, 29, 30, 31 ),
                           Array( 10, 21, 25, 31, 32 ),
                           Array( 14, 15, 18, 23, 24, 27, 30, 32 ),
                           Array( 18, 19, 22, 24, 26, 29, 31 ),
                           Array( 11, 20, 25, 28, 30, 32 ),
                           Array( 16, 19, 23, 31 ),
                           Array( 9, 18, 26, 28, 31, 32 ))

    val cost = Array(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3,
                     3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9)


    //
    // variables
    //
    val hire = Array.fill(num_workers)(CPIntVar(0 to 1)(cp))
    val total_cost = weightedSum(cost, hire)

    //
    // constraints
    //
    var numSols = 0
    cp.minimize(total_cost) subjectTo {

      // Sum the costs for hiring the qualified workers
      // and ensure that each task is covered.
      // (Also, make 0-base.).
      qualified.foreach(task=>
                        cp.add(sum(
                                   for {
                                     c <- 0 until task.length
                                   } yield hire(task(c)-1)
                                   ) >= 1
                               )
                        )
      
    } search {
       
      binaryMaxDegree(hire)
      
    } onSolution {
      
      println("\nSolution:")
      println("total_cost: " + total_cost)
      println("hire: " + hire.zipWithIndex.filter(_._1.value == 1).map(_._2).mkString(" "))

   }

   println(cp.start())

  }

}
