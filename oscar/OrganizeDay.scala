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

  Organizing a day in Oscar.

  Simple scheduling problem.

  Problem formulation from ECLiPSe:
   - Slides on (Finite Domain) Constraint Logic Programming, page 38f
   - http://eclipse-clp.org/reports/eclipse.ppt
 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object OrganizeDay {


  // No overlapping of tasks s1 and s2
  def noOverlap(s1: CPIntVar, d1: Int,
                s2: CPIntVar, d2: Int) = 
    (s1 + d1 <== s2) || (s2 + d2 <== s1)


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 4

    val Array(work, mail, shop, bank) = (0 to 3).toArray
    val tasks = Array(work, mail, shop, bank)
    val tasks_str = Array("work", "mail", "shop", "bank")

    val durations = Array(4,1,2,1)

    // precedences: 
    // task(t,0) mist be finished before task(t, 1)
    val before_tasks = Array(Array(bank, shop),
                             Array(mail, work))


    // the valid times of the day
    val begin = 9
    val end   = 17


    //
    // variables
    //
    val begins = Array.fill(n)(CPIntVar(begin to end)(cp))
    val ends   = Array.fill(n)(CPIntVar(begin to end)(cp))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(t <- tasks) {
        cp.add(ends(t) == begins(t) + durations(t))
      }

      for(i <- tasks;
          j <- tasks if i < j) {
        cp.add(noOverlap(begins(i), durations(i),
                         begins(j), durations(j)))
      }

      // specific constraints
      for(t <- 0 until before_tasks.length) {
        cp.add(ends(before_tasks(t)(0)) <= begins(before_tasks(t)(1)));
      }

      cp.add(begins(work) >= 11)

    } search {
       
      binaryMaxDegree(begins)
    } onSolution {
      
      for( t <- tasks) {
        println(tasks_str(t) + ":" + begins(t) + " -- " + durations(t) + "h --" + ends(t) )
      }
      println()

      numSols += 1

    } 
    println(cp.start())

  }

}
