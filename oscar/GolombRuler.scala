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
import Array._


/*

  Golomb Golomb ruler in Oscar.

  CSPLib problem 6
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob006/index.html
  """
  These problems are said to have many practical applications including 
  sensor placements for x-ray crystallography and radio astronomy. A 
  Golomb ruler may be defined as a set of m integers 
  0 = a_1 < a_2 < ... < a_m such that the m(m-1)/2 differences 
  a_j - a_i, 1 <= i < j <= m are distinct. Such a ruler is said to contain 
  m marks and is of length a_m. The objective is to find optimal (minimum 
  length) or near optimal rulers.

  Note that a symmetry can be removed by adding the constraint that 
  a_2 - a_1 < a_m - a_{m-1}, the first difference is less than the last. 
  """

  Also see:
  * http://mathworld.wolfram.com/GolombRuler.html
  * http://en.wikipedia.org/wiki/Golomb_ruler
  * http://www.research.ibm.com/people/s/shearer/grule.html


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object GolombRuler {

  def increasing(cp: CPSolver, y: Array[CPIntVar]) = {
    for (i <- 1 until y.length) {
      cp.add(y(i-1) <= y(i), Strong)
    }
  }

  def main(args: Array[String]) {
    
    val cp = CPSolver()

    //
    // data
    //
    var m = 8

    if (args.length > 0) {
      m = args(0).toInt
    }

    val n = m*m

    //
    // variables
    //
    val mark = Array.fill(m)(CPIntVar(0 to n)(cp))
    val differences = for{i <- 0 until m; j <- i+1 until m} yield mark(j)-mark(i)
                        
    //
    // constraints
    //
    var numSols = 0
    cp.minimize(mark(m-1)) subjectTo {

      cp.add(allDifferent(mark), Strong)
      cp.add(allDifferent(differences), Strong)

      increasing(cp, mark)

      // symmetry breaking
      cp.add(mark(0) == 0)
      cp.add(mark(1)-mark(0) < mark(m-1) - mark(m-2))

      // ensure positive differences 
      // (Cred to Pierre Schaus.)
      differences.foreach(d => cp.add(d > 0))

     } search {
 
       binaryStatic(mark) // 756 backtracks for m=8
     
     } onSolution {
       
       println("\nSolution:")
       print("mark: " + mark.mkString(""))
       println("\ndifferences: " + differences.mkString(""))
       println()

       numSols += 1

   }

   println(cp.start())

  }

}
