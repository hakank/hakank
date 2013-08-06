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


/**

  Sequence sum in Oscar.

  Sum of each sequence in s-slices in an array of n elements should be m.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object SequenceSum {

  // Sum the elements in y where each subsequence of length s
  // sums to m
  def sequence_sum(cp: CPSolver, y: Array[CPVarInt], m: CPVarInt, s: Int) = {
    val n = y.length
    for(i <- 0 until n - s + 1) {
      cp.add(sum( Range(i,i+s).map(j => y(j) ).toList) == m)
    }
    
  }

 
  def main(args: Array[String]) {

    val cp = CPSolver()

    val n = 6
    // val m = 10 // the sum
    val s = 3 // the sliding size


    // variables
    val x = Array.fill(n)(CPVarInt(cp, 1 to n))
    // the sum
    val m = CPVarInt(cp, 1 to n*n)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      sequence_sum(cp, x, m, s)
      cp.add(m == 10)
      
      // symmetry breaking
      // cp.add(x(0) == 1)
      
      
    } exploration {
      
      cp.binaryFirstFail(x)
        
      print("x: " + x.mkString(""))
      println("  m: " + m)
        
      numSols += 1
        
     } run()

     println("\nIt was " + numSols + " solutions.")
     cp.printStats()

   }

}
