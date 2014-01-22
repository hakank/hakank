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

  Set covering problem in Oscar.

  Minimize the number of security telephones in street
  corners on a campus.

  Example 9.1-2, page 354ff, from
  Taha 'Operations Research - An Introduction'


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCovering2 {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 8 // maximum number of corners
    val num_streets = 11 // number of connected streets

    // corners of each street
    // Note: 1-based (handled below)
    val corner = Array(Array(1,2),
                       Array(2,3),
                       Array(4,5),
                       Array(7,8),
                       Array(6,7),
                       Array(2,6),
                       Array(1,6),
                       Array(4,7),
                       Array(2,4),
                       Array(5,8),
                       Array(3,5))

    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(0 to 1)(cp))
    // number of telephones, to be minimized
    val z = sum(x)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      // ensure that all cities are covered
      for(i <- 0 until num_streets) {
        cp.add(x(corner(i)(0)-1) + x(corner(i)(1)-1) >= 1)
      }
      
    } search {
       
      binaryStatic(x)
    } onSolution {
      println("\nSolution:")
      println("x: " + x.mkString(""))
      println("z: " + z)

      numSols += 1

    }

    println(cp.start())

  }

}
