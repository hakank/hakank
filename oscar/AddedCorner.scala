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

  Added corner puzzle in Oscar.

  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the circles and 
  squares (one digit in each figure) so that the number in each square is equal 
  to the sum on the numbers in the circles which  adjoin it.  
  ...
  
     C F C
     F   F
     C F C
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object AddedCorner {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 8

    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(1 to n)(cp))
    val Array(a,b,c,d,e,f,g,h) = x

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {


      cp.add(allDifferent(x), Strong)
      cp.add(b == a + c)
      cp.add(d == a + f)
      cp.add(e == c + h)
      cp.add(g == f + h)

      
    } search {
      
      binaryFirstFail(x)

    } onSolution {
      
      println(a + " " + b   + " " + c)
      println(d + "   "     + " " + e)
      println(f + " " + g   + " " + h)
      println()
      numSols += 1  
      
    }

    println(cp.start())

  }

}
