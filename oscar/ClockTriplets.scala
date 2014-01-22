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

  Clock triplet problem in Oscar.

  Problem formulation
  http://www.f1compiler.com/samples/Dean 20Clark 27s 20Problem.f1.html
  """
  Dean Clark's Problem (Clock Triplets Problem)
 
  The problem was originally posed by Dean Clark and then presented
  to a larger audience by Martin Gardner. 
 
  The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
  by Timothy Rolfe. According to the article, in his August 1986 column for 
  Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
  
    Now for a curious little combinatorial puzzle involving the twelve
    numbers on the face of a clock. Can you rearrange the numbers (keeping
    them in a circle) so no triplet of adjacent numbers has a sum higher 
    than 21? This is the smallest value that the highest sum of a triplet
    can have.
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object ClockTriplets {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 12
    val triplet_sum = 21

    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(1 to n)(cp))


    //
    // constraints
    //
    cp.solve subjectTo {

      cp.add(allDifferent(x), Strong)

      cp.add(x(0) == 12)
      cp.add(x(1) > x(11))

      for(i <- 2 to 11) {
        cp.add(x(i) + x(i-1) + x(i-2) <= triplet_sum)
      }
      // and around the corners
      cp.add(x(10) + x(11) + x(0)  <= triplet_sum)
      cp.add(x(11) + x(0)  + x(1)  <= triplet_sum)
      
    } search {
       
      binaryMaxDegree(x)
      
    } onSolution {
      
      println("x:" + x.mkString(""))

      println("       " + x(0)                 )
      println("     " + x(11) + "    " + x(1)  )
      println("   " + x(10) + "       " + x(2) )
      println("  " + x(9)  + "         " + x(3))
      println("   " + x(8)  + "        " + x(4))
      println("     " + x(7)  + "    " + x(5)  )
      println("       " + x(6)                 )
      println()


    } 

    println(cp.start())


  }

}
