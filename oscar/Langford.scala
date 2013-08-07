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

  Langford's number problem in Oscar.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object Langford {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val k = if (args.length > 0) args(0).toInt else 4;
    val num_to_show = if (args.length > 1) args(1).toInt else 0;

    //
    // variables
    //
    val position = Array.fill(2*k)(CPVarInt(cp, 0 to 2*k-1))

    // channel positions to a solution array
    val solution = Array.fill(2*k)(CPVarInt(cp, 1 to k))

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {
      
      cp.add(allDifferent(position), Strong)
  
      for(i <- 1 to k) {
        cp.add(position(i+k-1) == (position(i-1) + i+1))
        cp.add(solution(position(i-1)) == i)
        cp.add(solution(position(k+i-1)) == i)
      }

      // symmetry breaking
      cp.add(solution(0) < solution(2*k-1))

    } exploration {
       
      cp.binary(position, _.size, _.min)

      print("solution:" + solution.mkString("") + " ")
      println("position:" + position.mkString(""))

      numSols += 1

      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop()
      }

   } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
