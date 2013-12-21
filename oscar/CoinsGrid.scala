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

  Coins grid puzzle in Oscar.

  Problem from 
  Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf

  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
  way that the following conditions are fulfilled:
     1. In each row exactly 14 coins must be placed.
     2. In each column exactly 14 coins must be placed.
     3. The sum of the quadratic horizontal distance from the main diagonal of all cells
        containing a coin must be as small as possible.
     4. In each cell at most one coin can be placed.
  The description says to place 14x31 = 434 coins on the chessboard each row containing 14
  coins and each column also containing 14 coins.
  """

  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object CoinsGrid {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    val n = 10 // original problem: 31
    val c = 4  // original problem: 14


    //
    // variables
    //

    val x = Array.fill(n,n)(CPVarInt(cp, 0 to 1))

    // quadratic horizonal distance (to minimize)
    val z = sum(List.tabulate(n)(i => List.tabulate(n)(j => x(i)(j)*abs(i-j)*abs(i-j) )).flatten)

    //
    // constraints
    //
    cp.minimize(z) subjectTo {
      
      // sum rows = c
      List.tabulate(n)(i => cp.add(sum(List.tabulate(n)(j => x(i)(j) )) == c))
      // sum cols = c
      List.tabulate(n)(i => cp.add(sum(List.tabulate(n)(j => x(j)(i) )) == c))


    } search {
       
      binaryFirstFail(x.flatten.toSeq)
    } onSolution {
      
      println("\nz:" + z)
      print("x:\n")
      for(i <- 0 until n) {
        println(x(i).mkString(" "))
      }

      

   }

   println(cp.start())

  }

}
