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

  Safe cracking puzzle in Oscar.

  From the Oz Primer:
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The code of Professor Smart's safe is a sequence of 9 distinct
  nonzero digits C1 .. C9 such that the following equations and
  inequations are satisfied:

        C4 - C6   =   C7
   C1 * C2 * C3   =   C8 + C9
   C2 + C3 + C6   <   C8
             C9   <   C8

   and

   C1 <> 1, C2 <> 2, ..., C9 <> 9

  can you find the correct combination?
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SafeCracking {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 9

    //
    // variables
    //
    val x   = Array.fill(n)(CPIntVar(1 to n)(cp))
    val Array(c1,c2,c3,c4,c5,c6,c7,c8,c9) = x

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {

      cp.add(allDifferent(x))
      cp.add(c4 - c6 == c7)
      cp.add(c1 * c2 * c3 == c8 + c9)
      cp.add(c2 + c3 + c6 < c8)
      cp.add(c9 < c8)

      (0 until n).foreach(i=> cp.add(x(i) != i+1))
      
    } search {
       
      binaryFirstFail(x)
    } onSolution {
      println("x:" + x.mkString(""))

      numSols += 1

   } 
   println(cp.start())

  }

}
