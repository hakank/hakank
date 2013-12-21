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

  Also see:
    - FurnitureMoving.scala: using a decomposition of cumulative
    - FurnitureMovingScheduling2.scala: a more compact version of this model

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object FurnitureMovingScheduling {

  def main(args: Array[String]) {

    //
    // data
    //
    val n = 4

    val Activities = 0 until n

    val durations = Array(30,10,15,15)
    val resources = Array(3,1,3,2)
    val capa = 4 // capacity, number of workers

    val horizon = durations.sum
    val cp = CPScheduler(horizon)

    //
    // variables
    //
    val resource  =  MaxResource(cp, capa, "FurnitureMoving")
    val activities = Array.tabulate(n)(a=>Activity(cp, durations(a)))
    for(a <- Activities) {
      activities(a) needs resources(a) ofResource resource
    }
    
    val makespan = cp.makespan

    //
    // constraints
    //

    cp.minimize(makespan) subjectTo{ 

    } search {
       
      setTimes(activities.map(_.start),activities.map(_.dur),activities.map(_.end))
    } onSolution {
      
      println("\nSolution:")

      println("makespan : " + makespan)
      println("capacity : " + resource.capacity)
      // some different ways of obtaining detailed info about activities
      println("resource.activities:\n" + activities.mkString("\n"))
      println("cp.activities:\n" + cp.activities.mkString("\n"))
      println("activities:\n" + activities.mkString("\n"))
      println(activities.map(a=>"Activity " + a.id + ":" + a.start + " --" + a.dur + " -- " + a.end).mkString("\n"))
      println()


    }

    println(cp.start())

  }

}
