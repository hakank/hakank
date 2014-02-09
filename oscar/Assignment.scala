/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Simple assignment problem in Oscar.

  From Wayne Winston "Operations Research",
  Assignment Problems, page 393f
  (This is a generalized version with an added test column)

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object Assignment extends CPModel with App {

  // Nicer syntax for a CPIntVar matrix 
  implicit class CPIntVarMatrix(val m: Array[Array[CPIntVar]]) {
    def row(r: Int): Array[CPIntVar] = m(r)
    def col(c: Int): Array[CPIntVar] = for { r <- 0 until m.length } yield m(r)(c)
  }

  def makeCPIntVarMatrix(rows: Int, cols: Int, range: Range) =
    Array.fill(rows)(Array.fill(cols)((CPIntVar(range))))

  //
  // data
  //

  // Problem instance
  // hakank: I added the fifth column to make it more
  //         interesting
  val rows = 4
  val cols = 5

  val ROWS = 0 until rows
  val COLS = 0 until cols

  val cost = Array(Array(14, 5, 8, 7, 15),
    Array(2, 12, 6, 5, 3),
    Array(7, 8, 3, 9, 7),
    Array(2, 4, 6, 10, 1))

  //
  // variables
  //
  val x = makeCPIntVarMatrix(rows, cols, 0 to 1)
  val total_cost = weightedSum(cost, x)

  //
  // constraints
  //
  var numSols = 0

  // Exacly one assignment per row (task),
  ROWS.foreach(i => add(sum(x.row(i)) == 1))

  // At most one assignments per column (worker)
  COLS.foreach(j => add(sum(x.col(j)) <= 1))

  minimize(total_cost) search { binaryMaxDegree(x.flatten.toSeq) }

  onSolution {
    println("total_cost:" + total_cost)
    for (i <- ROWS) {
      println(x(i).mkString(""))
    }
    println()
    for (i <- ROWS) {
      println("Task" + i + " is done by " +
        x(i).zipWithIndex.filter(_._1.value == 1).map(_._2).mkString(""))
    }
    println()

    numSols += 1
  } start ()

  println("\nIt was " + numSols + " solutions.")
}
