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
import java.util.Random


/*

  Appointment scheduling (set approach) in in Oscar.

  From Stack Overflow 
  Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)
  http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
  """
  Problem statement
   
  We have one employer that wants to interview N people, and therefore makes N 
  interview slots. Every person has a free-busy schedule for those slots. Give an 
  algorithm that schedules the N people into N slots if possible, and return a 
  flag / error / etc if it is impossible. What is the fastest possible runtime complexity?
  
  My approaches so far

  Naive: there are N! ways to schedule N people. Go through all of them, for each 
  permutation, check if it's feasible. O( n! )
  
  Backtracking:

  1. Look for any interview time slots that can only have 1 person. Schedule the person, 
     remove them from the list of candidates and remove the slot.
  2. Look for any candidates that can only go into 1 slot. Schedule the person, remove 
     them from the list of candidates and remove the slot.
  3. Repeat 1 & 2 until there are no more combinations like that.
  4. Pick a person, schedule them randomly into one of their available slots. Remember 
     this operation.
  5. Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we 
     have a schedule, return it. If there's an unresolvable conflict, backtrack.

  This is O( n! ) worst case, I think - which isn't any better.

  There might be a D.P. solution as well - but I'm not seeing it yet.

  Other thoughts

  The problem can be represented in an NxN matrix, where the rows are "slots", columns 
  are "people", and the values are "1" for free and "0" for busy. Then, we're looking for 
  a row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for 
  a row or a column with only one "1". (If the rank of the matrix is = N, I that means that 
  there is a solution. But the opposite does not hold) Another way to look at it is to 
  treat the matrix as an unweighed directed graph edge matrix. Then, the nodes each 
  represent 1 candidate and 1 slot. We're then looking for a set of edges so that every 
  node in the graph has one outgoing edge and one incoming edge. Or, with 2x nodes, it would 
  be a bipartite graph.
  
  Example of a matrix:

     1 1 1 1
     0 1 1 0
     1 0 0 1
     1 0 0 1
  """

  This model implements the set based approach mentioned in my answer to the
  question. 

  Also see these models:
  - MiniZinc: http://www.hakank.org/minizinc/appointment_scheduling_set.mzn
  - or-tools/C#: http://www.hakank.org/or-tools/appointment_scheduling_set.cs


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object AppointmentSchedulingSet {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // Original problem
    /*
    val s = Array(
                  Array(1,2,3,4),
                  Array(2,3),
                  Array(1,4),
                  Array(1,4)
                  )
    */

    val n = if (args.length > 0) args(0).toInt else 10;
    val limit = if (args.length > 1) args(1).toFloat else 0.6;
    val num_to_show = if (args.length > 2) args(2).toInt else 0;

    println("n: " + n + " limit: " + limit + " num_to_show: " + num_to_show)

    val rand = new Random(System.currentTimeMillis());
    val s = Array.fill(n)(
                          (for{i <- 1 to n if rand.nextFloat() >= limit} yield i).toArray
                          )

    println("Generated " + n + " time slots")

    if (n <= 30) {
      for(i <- 0 until n) {
        println("p #" + i + ": " + s(i).mkString(" "))
      }
    }
  

    //
    // variables
    //

    // The assignment of persons to a time slot (appointment number 1..n).
    val x = Array.tabulate(n)(i=>CPIntVar(s(i).toSet)(cp))


    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo { 
      
      cp.add(allDifferent(x), Strong)

    } search {
       
      binaryMaxDegree(x)

    } onSolution {
     
      numSols += 1      
      println("\nSolution #" + numSols + ": " + x.mkString(" "))  
   
    } 
    
    val stats = cp.start(num_to_show)

    println("\nIt was " + numSols + " solutions.")
    println(stats)
  }
}
