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


/**
 *
 * Decomposition of the global constraint allDifferent_except_0 in in Oscar.
 * 
 *  From Global constraint catalogue:
 * http://www.emn.fr/x-info/sdemasse/gccat/CallDifferent_except_0.html
 *  """ 
 * Enforce all variables of the collection VARIABLES to take distinct 
 * values, except those variables that are assigned to 0.
 * 
 * Example
 *    (<5, 0, 1, 9, 0, 3>)
 * 
 * The allDifferent_except_0 constraint holds since all the values 
 * (that are different from 0) 5, 1, 9 and 3 are distinct.
 * """
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object AllDifferentExcept0 {

  // Decomposition of allDifferent_except_0
  def allDifferent_except_0(cp: CPSolver, y: Array[CPVarInt]) = {

    for(i <- 0 until y.length; j <- 0 until i) {
      cp.add( ((y(i) !== 0) && (y(j) !== 0)) ==> (y(i) !== y(j)) )
    }
  }

  // Decomposition of increasing
  def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
    for (i <- 1 until y.length) {
      cp.add(y(i-1) <= y(i))
    }
  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = if (args.length > 0) args(0).toInt else 7

    // variables
    val x = Array.fill(n)(CPVarInt(cp, 0 to n))
    val occurrences = Array.tabulate(n+1)(i => (i,CPVarInt(cp, 0 to n)))
    val z = occurrences(0)._2  // the tuple is (#occurrences, value)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      allDifferent_except_0(cp, x)

      // Just for fun, we add that x should be increasing
      increasing(cp, x)

      // and that there must be exactly 2 0's
      cp.add(gcc(x, occurrences))
      cp.add(z == 2)


     } exploration {
       
       cp.binaryFirstFail(x)

       println("x:" + x.mkString(""))
       println("z:" + z)
       println()

       numSols += 1
       
     } run()
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
