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

  A simple modulo decomposition in Oscar.

  As of writing, OscaR supports these two versions
  of modulo
    modulo(CPVarInt, Int, CPVarInt) : Constraint
    modulo(CPVarInt, Int, Int) : Constraint
  
  but not
    modulo(CPVarInt, CPVarInt, CPVarInt)

  here is an implementation (decomposition)
  of the latter version.

  This implementation is based on the ECLiPSe version
  mentioned in "A Modulo propagator for ECLiPSE"
  http://www.hakank.org/constraint_programming_blog/2010/05/a_modulo_propagator_for_eclips.html
  The ECLiPSe Prolog source code:
  http://www.hakank.org/eclipse/modulo_propagator.ecl

  Which in turn is inspired by MiniZinc's bounds consistency 
  predicate in
  http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn    

  A variant is implemented in the or-tools/C# model
  http://www.hakank.org/or-tools/divisible_by_9_through_1.cs


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object MyModulo {


  def mod(cp: CPSolver,
          x: CPVarInt,
          y: CPVarInt) : CPVarInt = {
    val mmin = min(x.min, y.min)
    val mmax = min(x.max, y.max)
    val r = CPVarInt(cp, mmin, mmax)
    myMod(cp, x, y, r) 
    r

  }

  //
  // decomposition of modulo constraint:
  //    x % y == r
  //
  def myMod(cp: CPSolver,
            x: CPVarInt,
            y: CPVarInt,
            r: CPVarInt) = {

    val lbx = x.min
    val ubx = x.max;
    val ubx_neg = -ubx;
    val lbx_neg = -lbx;
    val min_x = min(lbx, ubx_neg);
    val max_x = max(ubx, lbx_neg);

    val d = CPVarInt(cp, min_x, max_x)

    // r >= 0
    cp.add(r >= 0)

    // x*r >= 0
    cp.add( x*r >= 0)

    // -abs(y) < r
    cp.add(-y.abs() < r)

    // r < abs(y)
    cp.add(r < y.abs())

    // min_x <= d, i.e. d > min_x
    cp.add(d > min_x)

    // d <= max_x
    cp.add(d <= max_x)

    // x == y*d+r
    cp.add(x - (y*d + r) == 0)

  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to 9))
    val two = CPVarInt(cp, 2 to 2)
    val zero = CPVarInt(cp, 0 to 0)
    val one = CPVarInt(cp, 1 to 1)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(i <- 1 until n) {
        // myMod(cp, x(i), x(i-1), one)
        cp.add(mod(cp, x(i), x(i-1)) == one)
        // cp.add(modulo(x(i), x(i-1)), 1)
      }

    } search {
       
      binaryMaxDegree(x)
    } onSolution {
      println(x.mkString(" "))

      numSols += 1

    } 
    println(cp.start())

  }

}
