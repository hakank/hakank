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
 * SEND+MORE MONEY problem in Oscar.
 * 
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMoreMoney {

   def main(args: Array[String]) {

      val cp = CPSolver()

      // variables
      val S = CPIntVar(0 to 9)(cp)
      val E = CPIntVar(0 to 9)(cp)
      val N = CPIntVar(0 to 9)(cp)
      val D = CPIntVar(0 to 9)(cp)
      val M = CPIntVar(0 to 9)(cp)
      val O = CPIntVar(0 to 9)(cp)
      val R = CPIntVar(0 to 9)(cp)
      val Y = CPIntVar(0 to 9)(cp)

      val all = Array(S,E,N,D,M,O,R,Y)

      cp.solve subjectTo {

        // constraints
        cp.add(       S*1000 + E*100 + N*10 + D +
                      M*1000 + O*100 + R*10 + E ==
            M*10000 + O*1000 + N*100 + E*10 + Y)
        cp.add(S > 0)
        cp.add(M > 0)
        cp.add(allDifferent(all), Strong)

      } search {

         binaryFirstFail(all)
      } onSolution {
         println(all.mkString(""))

      } 
      println(cp.start())

  }

}
