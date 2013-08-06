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
 *
 * Implementing Xkcd knapsack problem in Oscar
 *
 * http://xkcd.com/287/
 *
 * Some amount (or none) of each dish should be ordered to
 * give a total of exact 15.05
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Xkcd {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val price  = Array(215, 275, 335, 355, 420, 580) // in cents
    val num_prices = price.length
    val total = 1505


    // variables
    val x = Array.fill(num_prices)(CPVarInt(cp, 0 to 10))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(weightedSum(price, x) == total)

    } exploration {
       
       cp.binaryFirstFail(x)

       println("x:" + x.mkString(" "))

       numSols += 1
       
    } run()

    println("\nIt was " + numSols + " solutions.\n")
    cp.printStats()

  }

}
