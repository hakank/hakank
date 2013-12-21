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
import oscar.cp.constraints._
import collection.mutable._
import scala.collection.JavaConversions._

/**
 *
 * Nurse rostering using regular in Oscar.
 *
 * This is a simple nurse rostering model using a DFA and
 * the built-in regular with Automaton.
 *
 * The DFA is from the Nurse Rostering example in
 * page 37 ff in the "MiniZinc tutorial"
 * http://www.g12.cs.mu.oz.au/minizinc/downloads/doc-1.5.1/minizinc-tute.pdf
 * The DFA is at page 39.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object NurseRosteringRegular {

  def maxDomNotbound(vars: Iterable[CPVarInt]): Iterable[(CPVarInt, Int)] = {
    val notbound = vars.filterNot(_.isBound)
    if (notbound.nonEmpty) {
      val sizeMax = notbound.map(_.getSize).max
      notbound.zipWithIndex.filter {
        _._1.getSize == sizeMax
      }
    } else {
      Iterable()
    }
  }
 


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    // 
    var numToShow = if (args.length > 0) args(0).toInt else Int.MaxValue

    // Note: If you change num_nurses or num_days,
    //       please also change the constraints
    //       on nurse_stat and/or day_stat.
    val num_nurses = 7
    val num_days = 14

    val nurses = 0 until num_nurses
    val days = 0 until num_days

    // the shifts
    val day_shift   = 0
    val night_shift = 1
    val off_shift   = 2

    // different usages of the shifts
    val shifts_a       = Array(day_shift, night_shift, off_shift)
    val shifts_r       = day_shift to off_shift
    val num_shifts     = shifts_a.length

     // The DFA is from MiniZinc Tutorial, Nurse Rostering example:
     // - one day off every 4 days
     // - no 3 nights in a row.
      
    // state, next state, input (base = 0)
    val transition_tuples = Array(Array(0,1,0),  // state 0: off
                                  Array(0,2,1),  
                                  Array(0,0,2),
                                  Array(1,3,0),  // state 1: dayshift 1
                                  Array(1,3,1),
                                  Array(1,0,2),
                                  Array(2,3,0),  // state 2: night shift 1
                                  Array(2,4,1),
                                  Array(2,0,2),
                                  Array(3,5,0),  // state 3: dayshift 2 / night 1
                                  Array(3,5,1),
                                  Array(3,0,2),
                                  Array(4,5,0),  // state 4: night 2
                                  Array(4,0,2),
                                  Array(5,0,2))  // dayshift 3 / day 1 after night 2

    val days_str = Array("d","n","o") // for presentation

    // the DFA (for regular)
    val initial_state = 0
    val accepting_states: Set[java.lang.Integer] = Set(0,1,2,3,4,5)
    val num_states  = 6
    val num_letters = shifts_a.length

    val automaton = new Automaton(num_states, num_letters, initial_state, accepting_states)
    transition_tuples.foreach(a => 
                              automaton.addTransition(a(0), a(1), a(2))
                              )


    //
    // variables
    //
    val x = Array.fill(num_nurses,num_days)(CPVarInt(cp, shifts_r))
    val day_stat = Array.fill(num_days,num_shifts)(CPVarInt(cp, nurses))
    val nurse_stat = Array.fill(num_nurses,num_shifts)(CPVarInt(cp, days))

    val all = x.flatten

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(n <- nurses) {
        cp.add(regular((for{d <- days} yield x(n)(d)), automaton))

        for(s <- shifts_a) {
          cp.add(nurse_stat(n)(s) == sum(for{d <- days} yield x(n)(d) === s))
        }
   
        // min..max workdays (either day or night shift)
        cp.add(sum(for{s <- day_shift to night_shift} yield nurse_stat(n)(s)) >= 7)
        cp.add(sum(for{s <- day_shift to night_shift} yield nurse_stat(n)(s)) <= 10)

      }

      for(d <- days) {
        for(s <- shifts_a) {
          cp.add(day_stat(d)(s) == sum(for{n <- nurses} yield(x(n)(d) === s)))
        }

        //
        // Some constraints for each day:
        //
        if (d % 7 == 5 || d % 7 == 6) {
          // weekend:
          cp.add(day_stat(d)(day_shift) == 2)
          cp.add(day_stat(d)(night_shift) == 1)
          cp.add(day_stat(d)(off_shift) == 4 )
        } else {
          // workdays:
          // - exactly 3 on day shift
          cp.add(day_stat(d)(day_shift) == 3)
          // - exactly 2 on night
          cp.add(day_stat(d)(night_shift) == 2)
          // - exactly 2 off duty
          cp.add(day_stat(d)(off_shift) == 2 )
        }

      }

     } search {

      binary(all, _.size, _.min)
     
     } onSolution {
     
       for(n <- nurses) {
        print("Nurse " + n + ": ")
        var wd = 0
        for(d <- days) {
          val v = x(n)(d).value
          print(days_str(v) + " ")
          if (v <= night_shift) {
            wd += 1
          }
        }

        println("#workdays:" + wd + "  d:" + nurse_stat(n)(day_shift) + "  n:" + nurse_stat(n)(night_shift) + "  o:" + nurse_stat(n)(off_shift))

      }

      println("\nDay stats:")
      println("        d  n  o")
      for(d <- days) {
        print("Day " + "%2d".format(d) + ":")
        for(s <- shifts_a) {
          print(day_stat(d)(s) + " ")
        }
        println()
      }
      println()

      numSols += 1

      
     } 
     
     println(cp.start())

   }

}
