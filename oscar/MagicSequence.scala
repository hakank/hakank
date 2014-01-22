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

  Magic sequence problem in Oscar.
  
  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object MagicSequence {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = if (args.length > 0) args(0).toInt else 10;
    val all_values = Array.tabulate(n)(i=> (i,CPIntVar(0 to n-1)(cp)))

    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(0 to n-1)(cp))

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {

      cp.add(weightedSum(0 to n, x) == n)

      cp.add(sum(x) == n)

      cp.add(gcc(x, all_values), Strong)
      for(i<- 0 until n) {
        cp.add(x(i) == all_values(i)._1)
      }

    } search {
       
      binary(x, -_.constraintDegree, _.min)
    } onSolution {
      
      println("\nSolution:")
      println("x: " + x.mkString(" "))

      numSols += 1

   } 
   println(cp.start())

  }

}
