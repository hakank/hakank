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

  Bus scheduling in Oscar.
  
  Minimize number of buses in timeslots.

  Problem from Taha "Introduction to Operations Research", page 58.
   
  Note: This is a slightly more general model than Taha's.
 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object BusSchedule {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val time_slots = 6
    // min number of buses for each time slot
    val demands = Array(8, 10, 7, 12, 4, 4)
    val max_num = demands.sum


    //
    // variables
    //
 
    // How many buses start the schedule at time slot t
    val x = Array.fill(time_slots)(CPVarInt(cp, 0 to max_num))
    // Total number of buses
    val num_buses  = sum(x)

    //
    // constraints
    //
    var numSols = 0

    cp.minimize(num_buses) subjectTo {

      // Meet the demands for this and the next time slot.
      for(i <- 0 until time_slots - 1) {
        cp.add(x(i)+x(i+1) >= demands(i))
      }

      // The demand "around the clock"
      cp.add(x(time_slots-1) + x(0) - demands(time_slots-1) == 0)
      
      
    } exploration {
       
      cp.binary(x)

      println("\nSolution:")

      println("x: " + x.mkString(""))
      println("num_buses : " + num_buses)
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
