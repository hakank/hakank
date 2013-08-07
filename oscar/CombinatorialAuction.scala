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

  Combinatorial Auction in Oscar.
    
  This is a more general model for the combinatorial example
  in the Numberjack Tutorial, pages 9 and 24 (slides  19/175 
  and 51/175).

 
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object CombinatorialAuction {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    // the items for each bid
    val items = Array(
                      Array(0,1),   // A,B
                      Array(0,2),   // A, C
                      Array(1,3),   // B,D
                      Array(1,2,3), // B,C,D
                      Array(0))     // A

    val bid_ids = Array(0,1,2,3)
    val bid_amount = Array(10,20,30,40,14)

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to 1))
    val z = weightedSum(bid_amount, x)

    //
    // constraints
    //
    var numSols = 0

    cp.maximize(z) subjectTo {

      for(bid_id <- bid_ids) {
        cp.add(sum(for{item <- 0 until n
                       i <- 0 until items(item).length
                       if items(item)(i) == bid_id} yield x(item)
                   ) <= 1)

      }

    } exploration {
       
      cp.binary(x)

      println("\nSolution:")
      println("z:" + z)
      println("x:" + x.mkString(""))
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
