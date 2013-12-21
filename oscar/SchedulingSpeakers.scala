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

/**
 *
 * Scheduling speakers problem in Oscar
 *
 * From Rina Dechter, Constraint Processing, page 72
 * Scheduling of 6 speakers in 6 slots.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SchedulingSpeakers {


   def main(args: Array[String]) {

      val cp = CPSolver()

      //
      // data
      // 
      val num_speakers = 6
      val num_slots    = 6

      // Slots available to speakers
      val available = Array(
                            //                    Reasoning:
                            Array(3,4,5,6),    // 2) the only one with 6 after speaker F -> 1
                            Array(3,4),        // 5) 3 or 4
                            Array(2,3,4,5),    // 3) only with 5 after F -> 1 and A -> 6
                            Array(2,3,4),      // 4) only with 2 after C -> 5 and F -> 1
                            Array(3,4),        // 5) 3 or 4
                            Array(1,2,3,4,5,6) // 1) the only with 1
                            )


      //
      // decision variables
      // 
      val speakers = Array.tabulate(num_speakers)(i=>CPVarInt(cp, available(i).toSet ))

      var numSols = 0
      cp.solve subjectTo {

        cp.add(allDifferent(speakers))


      } search {

        binaryStatic(speakers)
      } onSolution {
        println(speakers.mkString(""))

        numSols += 1
      } 
      println(cp.start())

  }

}
