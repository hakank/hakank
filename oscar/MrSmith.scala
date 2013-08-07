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

  Mr Smith problem in Oscar.

  From an IF Prolog example (http://www.ifcomputer.de/)
  """
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
   o If Mr Smith comes, his wife will come too.
   o At least one of their two sons Matt and John will come.
   o Either Mrs Smith or Tim will come, but not both.
   o Either Tim and John will come, or neither will come.
   o If Matt comes, then John and his father will
     also come.
  """
  
  The answer should be:
  Mr_Smith_comes      =  0
  Mrs_Smith_comes     =  0
  Matt_comes          =  0
  John_comes          =  1
  Tim_comes           =  1


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object MrSmith {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 5

    //
    // variables
    // 
    // The matrix
    val x = Array.fill(n)(CPVarInt(cp, 0 to 1))
    val Array(mr_smith, mrs_smith, matt, john, tim) = x

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // If Mr Smith comes then his wife will come too.
      cp.add((mr_smith === 1) ==> (mrs_smith === 1))

      // At least one of their two sons Matt and John will come.
      cp.add((matt === 1 or john ===1))

      // Either Mrs Smith or Tim will come but not both.
      cp.add(mrs_smith + tim == 1)
      
      // Either Tim and John will come or neither will come.
      cp.add(tim == john)
      
      // If Matt comes then John and his father will also come.
      cp.add((matt === 1) ==> ((john === 1) && (mr_smith === 1)))


    } exploration {
       
      cp.binary(x)

      println("\nSolution:")
      println(x.mkString(""))
      println("Mr Smith : " + mr_smith)
      println("Mrs Smith: " + mrs_smith)
      println("Matt     : " + matt)
      println("John     : " + john)
      println("Tim      : " + tim)

      numSols += 1

   } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
