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

 Young tableaux and partition in Oscar.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

  1.   1 2 3 4

  2.   1 2 3         1 2 4    1 3 4
       4             3        2

  3.   1 2           1 3
       3 4           2 4

  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3

  5.   1
       2
       3
       4
  """  

 @author Hakan Kjellerstrand hakank@gmail.com
 http://www.hakank.org/oscar/
 
*/
object YoungTableaux {

  def main(args: Array[String]) {

    val cp = CPSolver()

    val n = if (args.length > 0) args(0).toInt else 4;

    val RANGE0 = 0 until n
    val RANGE1 = 1 until n

    // variables

    // grid
    val x = Array.fill(n,n)(CPVarInt(cp, 1 to n+1))
    val x_flatten = x.flatten

    // the partition structure
    val p = Array.fill(n)(CPVarInt(cp, 0 to n+1))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // 1..n is used exactly once
      for(i <- 1 until n+1) {
        cp.add(gcc(x_flatten, i to i, 1, 1), Strong) 
      }
      
      cp.add(x(0)(0) == 1)

      // rows
      for(i <- RANGE0;
          j <- RANGE1) {
          cp.add(x(i)(j) >= x(i)(j-1))
      }

      // columns
      for(j <- RANGE0;
          i <- RANGE1) {
          cp.add(x(i)(j) >= x(i-1)(j))
      }

      // calculate the structure (the partition)
      for(i <- RANGE0) {
        cp.add(p(i) == sum(for(j <- RANGE0) yield (x(i)(j) <== n)))
      }
      
      cp.add(sum(p) == n)

      for(i <- RANGE1) {
        cp.add(p(i-1) >= p(i))
      }


    } search {
      binaryFirstFail(x.flatten.toSeq)
    } onSolution {
      println("\nSolution:")
      print("p: ")
      for(i <- RANGE0) {
        print(p(i) + " ")
      }
      println()
      for(i <- RANGE0) {
        var c = 0 // number of non-empty items
        for(j <- RANGE0) {
          val v = x(i)(j).value
          if (v <= n) {
            print(v + " ")
            c += 1
          }
        }
        // just print non-empty lines
        if (c > 0) {
          println()
        }
      }

      numSols += 1

    } 

    println(cp.start())

  }

}
