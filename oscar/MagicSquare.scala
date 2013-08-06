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
import scala.math._

/**

  Magic square in Oscar.


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object MagicSquare {

 
  def main(args: Array[String]) {

    val cp = CPSolver()

    val n = if (args.length > 0) args(0).toInt else 4;
    val num_to_show = if (args.length > 1) args(1).toInt else 0;

    val n2 = n*n

    println("n:" + n + " num_to_show: " + num_to_show)

    //
    // variables
    //
    val x = Array.fill(n,n)(CPVarInt(cp, 1 to n2))
    val x_t = x.transpose

    // val total = CPVarInt(cp, 1 to n*n*n)
    val total = (n * (n*n + 1) / 2)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

       cp.add(allDifferent(x.flatten), Strong)

       // rows and columns
       for(i <- 0 until n) {
         cp.add(sum(x(i)) == total)
         cp.add(sum(x_t(i)) == total)
       }
  
       // diagonals
       cp.add(sum( for(i <- 0 until n) yield x(i)(i)) == total)
       cp.add(sum( for(i <- 0 until n) yield x(i)(n-i-1) ) == total)

       // symmetry breaking
       cp.add(x(0)(0)   < x(0)(n-1))
       cp.add(x(0)(n-1) < x(n-1)(0))
       cp.add(x(0)(0)   < x(n-1)(n-1))


     } exploration {
       
       cp.binary(x.flatten, _.size, _.min)

       println("\nSolution:\ntotal " + total)
       for(i <- 0 until n) {
         println(x(i).map(j=>"%3d".format(j.value)).mkString(""))
       }
       println()


       numSols += 1

       if (num_to_show > 0 && numSols >= num_to_show) {
         cp.stop()
       }
       
     } run()
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
