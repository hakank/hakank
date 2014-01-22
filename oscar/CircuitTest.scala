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

import oscar.cp.core.CPIntVar
import scala.io.Source._
import scala.math._

/*

  Decomposition of circuit constraint in Oscar.

  Here we implement a decomposition of circuit (circuit_me)
  and also a path extraction for the circuit (circuit_path).

  Please note that Oscar has a built-in circuit/1. This
  model is just an etude...

  Comparison (where the results redirects to a file)

  Using my decomposition (circuit_me) took 47.103s
  with these statistics:

    It was 362880 solutions.
    (time(ms),46091)
    (#bkts,362879)
    (time in fix point(ms),15771)
    (time in trail restore(ms),6487)
    (max trail size,2076)


  Using the built-in circuit (circuit) took 44.652
  with these statistics (slightly better):

     It was 362880 solutions.
     (time(ms),43639)
     (#bkts,362879)
     (time in fix point(ms),13336)
     (time in trail restore(ms),5682)
     (max trail size,1181)

  Without any output of the solutions it's much faster (~ factor 2).

  My decomposition (circuit_me) took 25.178s
     It was 362880 solutions.
     (time(ms),24232)
     (#bkts,362879)
     (time in fix point(ms),14445)
     (time in trail restore(ms),6485)
     (max trail size,2076)

  Built-in (circuit) took 22.406s
     It was 362880 solutions.
     (time(ms),21439)
     (#bkts,362879)
     (time in fix point(ms),12849)
     (time in trail restore(ms),5674)
     (max trail size,1181)


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object CircuitTest {

  /*
   * circuit_me(x) 
   *
   * Ensures that x is an circuit.
   *
   * Note: the domain of x must be 1..n, where n is the
   * length of x
   * 
   */
  def circuit_me(cp: CPSolver, x: Array[CPIntVar]) = {

    val len = x.length
    val z = Array.tabulate(len)(i => CPIntVar(0 to len - 1)(cp))

    cp.add(allDifferent(x), Strong)
    cp.add(allDifferent(z), Strong)

    cp.add(z(0) == x(0))

    // then put the orbit of x(0) in z(1..n-1)
    for (i <- 1 until len) {
      cp.add(x(z(i - 1)) == z(i), Strong)
    }

    // for(i <- lb until len-1) {
    //    cp.add(z(i) != 0)
    // }

    cp.add(z(len - 1) == 0)

  } // end circuit_me

  /*
   * circuit_path(x, p)
   * 
   * Extract the path p from a circuit x.
   * We assume that the path starts from 1
   * 
   */
  def circuit_path(cp: CPSolver, x: Array[CPIntVar], p: Array[CPIntVar]) = {

    val len = x.length

    cp.add(allDifferent(p), Strong)
    cp.add(p(0) == 0) // path always starts with 1
    for (i <- 1 until len) {
      cp.add(x(p(i - 1)) == p(i), Strong)
    }

  } // end circuit_path

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6

    //
    // variables
    //

    // Note: Here we use domain of 0..n-1
    val x = Array.fill(n)(CPIntVar(0 to n - 1)(cp))
    val p = Array.fill(n)(CPIntVar(0 to n - 1)(cp))

    //
    // constraints
    //

    cp.solve subjectTo {

      // cp.add(circuit(x), Strong) // use the built-in
      circuit_me(cp, x)
      circuit_path(cp, x, p)

    } search {

      binaryFirstFail(x)
    } onSolution {

      println("\nCircuit: " + x.mkString(""))
      println("Path   : " + p.mkString(""))

    }

    println(cp.start())
  }

}
