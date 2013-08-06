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

import oscar.cp.scheduling._
import oscar.cp.core._
import oscar.cp.constraints._
import oscar.visual._

import scala.io.Source._
import scala.math._

/*

  Furniture Moving (scheduling) problem in Oscar.

  Problem from Marriott & Stuckey: 'Programming with constraints', page  112f

  This model uses OscaR's Scheduling API CumulativeResource.

  This is a more compact version of FurnitureMovingScheduling.scala.
  (inspired by the example oscar.examples.cp.scheduling.RCPSP)
 
  Also see:
    - FurnitureMoving.scala: using a decomposition of cumulative

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object FurnitureMovingScheduling2 {

  def main(args: Array[String]) {

    //
    // data
    //
    val n = 4

    val Activities = 0 until n

    val durations = Array(30,10,15,15)
    val resources = Array(3,1,3,2)
    // (durations, consumption)
    val instance = durations.zip(resources) 
    val capa = 4 // capacity, number of workers

    val horizon = durations.sum
    val cp = CPScheduler(horizon)

    //
    // variables
    //
    val resource  =  MaxResource(cp, capa, "FurnitureMoving")
    instance.map(a => Activity(cp, a._1) needs a._2 ofResource resource)
    
    val makespan = cp.makespan

    //
    // constraints
    //
    var numSols = 0

    cp.minimize(makespan) subjectTo{ 
      

    } exploration {
       
      cp.setTimes(cp.activities)

      println("\nSolution:")

      println("makespan : " + makespan)
      println("capacity : " + resource.capacity)
      println("cp.activities:\n" + cp.activities.mkString("\n"))
      println()

      numSols += 1

    }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
