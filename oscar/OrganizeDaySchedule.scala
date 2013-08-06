/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._
import oscar.cp.scheduling._
import scala.io.Source._
import scala.math._

/*

  Organizing a day in Oscar.

  Simple scheduling problem.

  Problem formulation from ECLiPSe:
   - Slides on (Finite Domain) Constraint Logic Programming, page 38f
   - http://eclipseclp.org/reports/eclipse.ppt
 
  Cf OrganizeDay.scala
  This version use the Scheduling API.

  Thanks Renaud for a much neater version.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object OrganizeDaySchedule {

	def main(args : Array[String]) {

		//
		// data
		//

		// the valid times of the day
		val begin = 9
		val end = 17

		val horizon = end - begin
		val cp = CPScheduler(horizon)

		println("horizon: " + horizon)

		//
		// variables
		//
		val work = Activity(cp, 4, "Work")
		val mail = Activity(cp, 1, "Mail")
		val shop = Activity(cp, 2, "Shop")
		val bank = Activity(cp, 1, "Bank")

		val resource   = UnitResource(cp)
		val activities = cp.activities

		for (activity <- activities) {
			activity needs resource
		}

		val makespan = cp.makespan

		//
		// constraints
		//
		var numSols = 0

		cp.solve subjectTo {

			// precedences
			bank precedes shop
			mail precedes work

            cp.add(work.start >= 11 - begin)

		} exploration {

			cp.setTimes(cp.activities)
			//cp.binaryFirstFail(cp.activities)

			println("makespan : " + makespan)
			println("criticality: " + resource.criticality)
			println(activities.mkString("\n"))
			println(activities.map(a => "Activity " + a.name + ": " + (a.start.value + begin) + " --" + a.dur + "h -- " + (a.end.value + begin)).mkString("\n"))

			println()

			numSols += 1
		} run()

		println("\nIt was " + numSols + " solutions.")
		cp.printStats()
	}
}
