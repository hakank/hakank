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
import oscar.cp.scheduling._
import scala.io.Source._
import scala.math._

/*

  Building a house (scheduling) in Oscar.

  This model is adapted from OPL model sched_intro.mod (examples).
  """
  This is a basic problem that involves building a house. The masonry,
  roofing, painting, etc.  must be scheduled. Some tasks must
  necessarily take place before others, and these requirements are
  expressed through precedence constraints.
  """

  This model uses OscaR's Scheduling API.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object BuildingAHouseScheduling {

	def main(args : Array[String]) {

		//
		// data
		//
		val n = 10
		val duration = Array(35, 15, 40, 15, 5, 10, 5, 10, 5, 5)
		val capa = 3
		val horizon = duration.sum
		val cp = CPScheduler(horizon)

		println("horizon: " + horizon)

		//
		// variables
		//
		val masonry   = Activity(cp, duration(0), "Masonry")
		val carpentry = Activity(cp, duration(1), "Carpentry")
		val plumbing  = Activity(cp, duration(2), "Plumbing")
		val ceiling   = Activity(cp, duration(3), "Ceiling")
		val roofing   = Activity(cp, duration(4), "Roofing")
		val painting  = Activity(cp, duration(5), "Painting")
		val windows   = Activity(cp, duration(6), "Windows")
		val facade    = Activity(cp, duration(7), "Facade")
		val garden    = Activity(cp, duration(8), "Garden")
		val moving    = Activity(cp, duration(9), "Moving")

		val resource = MaxResource(cp, capa, "BuildingAHouse")
		val activities = cp.activities

		for (activity <- activities) {
			activity needs 1 ofResource resource
		}

		val makespan = cp.makespan

		// extra constraint
		val zero = CPVarInt(cp, 0 to 0)
		val z = maximum(Array(moving.end - 100, zero)) * 400 +
			    maximum(Array(-masonry.start + 25, zero)) * 200 +
			    maximum(Array(-carpentry.start + 75, zero)) * 300 +
			    maximum(Array(-ceiling.start + 75, zero)) * 100

		//
		// constraints
		//

		// cp.minimize(makespan) subjectTo {
		cp.minimize(z) subjectTo {

			// precedences
			masonry precedes carpentry
			masonry precedes plumbing
			masonry precedes ceiling
			carpentry precedes roofing
			ceiling precedes painting
			roofing precedes windows
			roofing precedes facade
			plumbing precedes facade
			roofing precedes garden
			plumbing precedes garden
			windows precedes moving
			facade precedes moving
			garden precedes moving
			painting precedes moving

		} search {

			setTimes(cp.activities.map(_.start),cp.activities.map(_.dur),cp.activities.map(_.end))

		} onSolution {
		  
			println("makespan:" + makespan)
			println(activities.map(a => "%-10s".format(a.name) + ": " + "%3d".format(a.start.value) + " --" + "%3d".format(a.dur.value) + "h --" + "%3d".format(a.end.value)).mkString("\n"))
			println()
		  
		}

		println(cp.start())

	}

}
