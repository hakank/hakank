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

  Crew allocation problem in OscaR.
   
  From Gecode example crew
  examples/crew.cc
  """
  Example: Airline crew allocation
  
  Assign 20 flight attendants to 10 flights. Each flight needs a certain
  number of cabin crew, and they have to speak certain languages.
  Every cabin crew member has two flights off after an attended flight.
  """
  
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object Crew {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    // 
    var num_to_show = 1

    if (args.length > 0) {
      num_to_show = args(0).toInt
    }


    val names = Array("Tom",
                      "David",
                      "Jeremy",
                      "Ron",
                      "Joe",
                      "Bill",
                      "Fred",
                      "Bob",
                      "Mario",
                      "Ed",
                      "Carol",
                      "Janet",
                      "Tracy",
                      "Marilyn",
                      "Carolyn",
                      "Cathy",
                      "Inez",
                      "Jean",
                      "Heather",
                      "Juliet")

    val num_persons = names.length
    val PERSONS = 0 until num_persons

    //
    // Attributes of the crew
    //
    val attributes = Array(
                           // steward, hostess, french, spanish, german
                           Array(1,0,0,0,1),   // Tom     = 0
                           Array(1,0,0,0,0),   // David   = 1
                           Array(1,0,0,0,1),   // Jeremy  = 2
                           Array(1,0,0,0,0),   // Ron     = 3
                           Array(1,0,0,1,0),   // Joe     = 4
                           Array(1,0,1,1,0),   // Bill    = 5
                           Array(1,0,0,1,0),   // Fred    = 6
                           Array(1,0,0,0,0),   // Bob     = 7
                           Array(1,0,0,1,1),   // Mario   = 8
                           Array(1,0,0,0,0),   // Ed      = 9
                           Array(0,1,0,0,0),   // Carol   = 10
                           Array(0,1,0,0,0),   // Janet   = 11
                           Array(0,1,0,0,0),   // Tracy   = 12
                           Array(0,1,0,1,1),   // Marilyn = 13
                           Array(0,1,0,0,0),   // Carolyn = 14
                           Array(0,1,0,0,0),   // Cathy   = 15
                           Array(0,1,1,1,1),   // Inez    = 16
                           Array(0,1,1,0,0),   // Jean    = 17
                           Array(0,1,0,1,1),   // Heather = 18
                           Array(0,1,1,0,0)    // Juliet  = 19
                           )


    //
    // Required number of crew members.
    //
    // The columns are in the following order:
    // staff     : Overall number of cabin crew needed
    // stewards  : How many stewards are required
    // hostesses : How many hostesses are required
    // french    : How many French speaking employees are required
    // spanish   : How many Spanish speaking employees are required
    // german    : How many German speaking employees are required
    //
    val required_crew = Array(
                              Array(4,1,1,1,1,1), // Flight 1
                              Array(5,1,1,1,1,1), // Flight 2
                              Array(5,1,1,1,1,1), // ..
                              Array(6,2,2,1,1,1),
                              Array(7,3,3,1,1,1),
                              Array(4,1,1,1,1,1),
                              Array(5,1,1,1,1,1),
                              Array(6,1,1,1,1,1),
                              Array(6,2,2,1,1,1), // ...
                              Array(7,3,3,1,1,1)  // Flight 10
                              )

    val num_flights = required_crew.length
    val FLIGHTS = 0 until num_flights
    

    //
    // variables
    //
    val crew = Array.fill(num_flights,num_persons)(CPIntVar(0 to 1)(cp))
    val crew_flat = crew.flatten

    // val num_working = CPIntVar(cp, 1 to num_persons)
    val num_working = sum(for{p <- PERSONS}
                           yield sum(for{f <- FLIGHTS} yield (crew(f)(p)))>>= 0)

    //
    // constraints
    //

    cp.solve subjectTo {
    // cp.minimize(num_working) subjectTo {

      // cp.add(num_working <= 19)

      // requirements per flights
      for(f <- FLIGHTS) {
        // size of crew
        cp.add(sum(for{p <- PERSONS} yield crew(f)(p)) == required_crew(f)(0))
          
        // attributres and requirements
        for(a <- 0 until 5) {
          cp.add(sum(for{p <- PERSONS} yield crew(f)(p)*attributes(p)(a)) >= required_crew(f)(a+1))
        }
      }

      // after a flight, break for at least two flights
      for(f <- 0 until num_flights - 2) {
        for(p <- PERSONS) {
          cp.add(crew(f)(p) + crew(f+1)(p) + crew(f+2)(p) <= 1)
        }
      }

      // extra contraint: all must work at least two of the flights
      // Note: this don't do well with a minimize objective.
      /*
      for(p <- PERSONS) {
        cp.add(sum(for{f <- FLIGHTS} yield(crew(f)(p))) >= 2)
      }
      */

      // extra constraint: No one should work more than 4 flights
      /*
      for(p <- PERSONS) {
        cp.add(sum(for{f <- FLIGHTS} yield(crew(f)(p))) <= 4)
      }
      */


     } search {

        binaryStatic(crew_flat)
        
     } onSolution {
       
        println("num_working: " + num_working)
        for(f <- FLIGHTS) {
          for(p <- PERSONS) {
            print(crew(f)(p) + " ")
          }
          println()
        }

        println("\nFlights: ");
        for(f <- FLIGHTS) {
          print("Flight #" + f + ": ");
          for(p <- PERSONS) {
            if (crew(f)(p).value == 1) {
              print(names(p) + " ")
            }
          }
          println()
        }

        println("\nCrew:")
        for(p <- PERSONS) {
          print("%-10s".format(names(p)) + ": Flight(s): ")
          for(f <- FLIGHTS) {
            if (crew(f)(p).value == 1) {
              print(f + " ")
            }
          }
          println()
        }
        
        println()
        println();

     } 
     
     println(cp.start(nbSolMax = num_to_show))
    
   }

}
