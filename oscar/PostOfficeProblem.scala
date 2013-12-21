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

  Post office problem in Oscar.
  
  Problem statement:
  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

  From Winston 'Operations Research: Applications and Algorithms':
  """
  A post office requires a different number of full-time employees working
  on different days of the week [summarized below]. Union rules state that
  each full-time employee must work for 5 consecutive days and then receive
  two days off. For example, an employee who works on Monday to Friday
  must be off on Saturday and Sunday. The post office wants to meet its
  daily requirements using only full-time employees. Minimize the number
  of employees that must be hired.

  To summarize the important information about the problem:

  Every full-time worker works for 5 consecutive days and takes 2 days off
  - Day 1 (Monday): 17 workers needed
  - Day 2 : 13 workers needed
  - Day 3 : 15 workers needed
  - Day 4 : 19 workers needed
  - Day 5 : 14 workers needed
  - Day 6 : 16 workers needed
  - Day 7 (Sunday) : 11 workers needed
  *
  The post office needs to minimize the number of employees it needs
  to hire to meet its demand.
  """
  

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object PostOfficeProblem {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // days 0..6, monday 0
    val n = 7
    val need = Array(17, 13, 15, 19, 14, 16, 11)

    // Total cost for the 5 day schedule.
    // Base cost per day is 100.
    // Working saturday is 100 extra
    // Working sunday is 200 extra.
    val cost = Array(500, 600, 800, 800, 800, 800, 700)


    //
    // variables
    //
 
    // number of workers starting at day i
    val x = Array.fill(n)(CPVarInt(cp, 0 to need.max))
    val total_cost  = weightedSum(cost, x)
    val num_workers = sum(x)

    //
    // constraints
    //
    var numSols = 0

    cp.minimize(total_cost) subjectTo {

      for(i <- 0 until n) {
        cp.add(
               sum(
                   for{j <- 0 until n
                       if (j != (i+5) % n && j != (i+6) % n)
                   } yield x(j)
                   ) >= need(i)
               )
      }

      // Add a limit for the cost
      // cp.add(total_cost <= 20000)

      
    } search {
       
      binary(x,_.min, _.min)
    } onSolution {
      println("\nSolution:")

      println("x          : " + x.mkString(""))
      println("total_cost : " + total_cost)
      println("num_workers: " + num_workers)
      println()

      numSols += 1

    }

    println(cp.start())

  }

}
