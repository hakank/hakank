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
 * Least diff problem in Oscar.
 *
 * Minimize the difference ABCDE - FGHIJ
 * where A..J are distinct digits (0..9).
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object LeastDiff {

  def main(args: Array[String]) {

    val cp = CPSolver()

    // variables
    val A = CPIntVar(0 to 9)(cp)
    val B = CPIntVar(0 to 9)(cp)
    val C = CPIntVar(0 to 9)(cp)
    val D = CPIntVar(0 to 9)(cp)
    val E = CPIntVar(0 to 9)(cp)
    val F = CPIntVar(0 to 9)(cp)
    val G = CPIntVar(0 to 9)(cp)
    val H = CPIntVar(0 to 9)(cp)
    val I = CPIntVar(0 to 9)(cp)
    val J = CPIntVar(0 to 9)(cp)

    val all = Array(A, B, C, D, E, F, G, H, I, J)
    val X = A * 10000 + B * 1000 + C * 100 + D * 10 + E
    val Y = F * 10000 + G * 1000 + H * 100 + I * 10 + J
    val Diff = X - Y

    // constraints
    cp.minimize(Diff) subjectTo {

      cp.add(allDifferent(all), Strong)
      cp.add(A > 0)
      cp.add(F > 0)
      cp.add(Diff > 0)

    } search {

      binary(all ++ Array(X, Y, Diff), -_.constraintDegree, _.min)
      
    } onSolution {
      
      println(Array(A, B, C, D, E).mkString("") + " -" +
              Array(F, G, H, I, J).mkString("") + " =" +
              Diff)

    }

    println(cp.start())

  }

}
