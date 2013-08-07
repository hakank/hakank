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
import scala.math._

/**

  SEND+MORE=MONEY in "any" base in Oscar.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object SendMoreMoneyAnyBase {

 
  def main(args: Array[String]) {

    val cp = CPSolver()

    var base = 10
    
    if (args.length > 0) {
      base = args(0).toInt
    }

    println("Base: " + base)

    val base2 = base - 1

    val b1 = base
    val b2 = pow(base, 2).toInt
    val b3 = pow(base, 3).toInt
    val b4 = pow(base, 4).toInt

    //
    // variables
    //
    val S = CPVarInt(cp, 0 to base2)
    val E = CPVarInt(cp, 0 to base2)
    val N = CPVarInt(cp, 0 to base2)
    val D = CPVarInt(cp, 0 to base2)
    val M = CPVarInt(cp, 0 to base2)
    val O = CPVarInt(cp, 0 to base2)
    val R = CPVarInt(cp, 0 to base2)
    val Y = CPVarInt(cp, 0 to base2)

    val all = Array(S,E,N,D,M,O,R,Y)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

        cp.add(allDifferent(all), Strong)
        cp.add(       S*b3 + E*b2 + N*b1 + D +
                      M*b3 + O*b2 + R*b1 + E ==
               M*b4 + O*b3 + N*b2 + E*b1 + Y
             )
        cp.add(S > 0)
        cp.add(M > 0)


     } exploration {
       
       cp.binaryFirstFail(all)

       println(all.mkString(""))

       numSols += 1
       
     } run()

     println("\nIt was " + numSols + " solutions.")
     cp.printStats()

   }

}
