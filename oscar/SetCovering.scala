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
  
  Placing of firestations. 
  From Winston 'Operations Research', page 486.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCovering {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val min_distance = 15
    val num_cities = 6

    val distance = Array(Array( 0,10,20,30,30,20),
                         Array(10, 0,25,35,20,10),
                         Array(20,25, 0,15,30,20),
                         Array(30,35,15, 0,15,25),
                         Array(30,20,30,15, 0,14),
                         Array(20,10,20,25,14, 0))

    //
    // variables
    //
 
    val x = Array.fill(num_cities)(CPVarInt(cp, 0 to 1))
    val z = sum(x)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      // ensure that all cities are covered
      for(i <- 0 until num_cities) {
        cp.add(
               sum(
                   for{
                     j <- 0 until num_cities
                     if distance(i)(j) <= min_distance
                   } yield x(j)
                   ) >= 1
               )
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
