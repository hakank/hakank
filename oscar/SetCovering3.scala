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

  Set covering problem from
  Katta G. Murty: 'Optimization Models for Decision Making',
  page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCovering3 {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num_groups = 6
    val num_senators = 10

    // which group does a senator belong to?
    val belongs = Array(Array(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),   // 1 southern
                        Array(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),   // 2 northern
                        Array(0, 1, 1, 0, 0, 0, 0, 1, 1, 1),   // 3 liberals
                        Array(1, 0, 0, 0, 1, 1, 1, 0, 0, 0),   // 4 conservative
                        Array(0, 0, 1, 1, 1, 1, 1, 0, 1, 0),   // 5 democrats
                        Array(1, 1, 0, 0, 0, 0, 0, 1, 0, 1))  // 6 republicans

    //
    // variables
    //
 
    val x = Array.fill(num_senators)(CPVarInt(cp, 0 to 1))
    // number of assigned senators, to be minimized
    val z = sum(x)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      // ensure that each group is covered by at least
      // one senator
      for(i <- 0 until  num_groups) {
        cp.add(sum(
                   for{j <- 0 until num_senators
                   } yield x(j)*belongs(i)(j)
                   ) >= 1)
      }

      
    } search {
       
      binaryStatic(x)
    } onSolution {

      println("\nSolution:")
      println("z: " + z)
      println("x: " + x.mkString(""))
      
      // More details
      for(j <- 0 until num_senators if x(j).value == 1) {
          print("Senator " + (j+1) + " belongs to these groups: ")
          for(i <- 0 until num_groups if belongs(i)(j) == 1) {
              print((1+i) + " ")
          }
          println()
      }

      numSols += 1

    }

    println(cp.start())

  }

}
