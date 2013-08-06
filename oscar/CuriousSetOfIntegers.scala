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
 * Problem from Martin Gardner (February 1967):
 * """
 * The integers 1,3,8, and 120 form a set with a remarkable property: the
 * product of any two integers is one less than a perfect square. Find
 * a fifth number that can be added to the set without destroying
 * this property.
 * """
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object CuriousSetOfIntegers {


   def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
     for (i <- 1 until y.length) {
       cp.add(y(i-1) <= y(i))
     }
   }


   def main(args: Array[String]) {

      val cp = CPSolver()

      //
      // data
      // 
      val n = 5
      val max_val = 10000

      //
      // decision variables
      // 
      val x = Array.fill(n)(CPVarInt(cp, 0 to max_val))

      var numSols = 0
      cp.solve subjectTo {

	cp.add(allDifferent(x), Strong)

        for(i <- 0 until n - 1) {
          for(j <- i + 1 until n) {
            val p = CPVarInt(cp, 0 to max_val)
            cp.add((p*p- 1) == x(i) * x(j))
          }
        }

        // Symmetry breaking
        increasing(cp, x)

        // This is the original problem:
        // The given numbers are {1,3,8,120},
        // Which is the fifth number?
        cp.add(
               ( (x(0) <<= 1) && x(1) === 1 && x(2) === 3 && x(3) === 8 && x(4) === 120)
               ||
               (x(0) === 1 && x(1) === 3 && x(2) === 8 && x(3) === 120 && (x(4) >>= 120))
               )

      } exploration {

        cp.binary(x)

        println(x.mkString(""))

        val s = Set(1,3,8,120)
        for(i <- 0 until n) {
          val v = x(i).value
          if (!s.contains(v)) {
            println("The fifth number is " + v)
          }
        }

        numSols += 1
      } run()

      println("\nIt was " + numSols + " solutions.")	  
      cp.printStats()

  }

}
