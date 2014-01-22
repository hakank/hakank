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

  Set partition problem in Oscar.

  Problem formulation from
  http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
  This is a partition problem.
  Given the set S = {1, 2, ..., n},
  it consists in finding two sets A and B such that:

   A U B = S,
   |A| = |B|,
   sum(A) = sum(B),
   sum_squares(A) = sum_squares(B)
  """

  Note: This model uses a binary matrix to represent the sets.
  

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetPartition {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n        = if (args.length > 0) args(0).toInt else 16;
    val num_sets = if (args.length > 1) args(1).toInt else 2;

    val NRANGE = 0 until n
    val SRANGE = 0 until num_sets

    println("n: " + n + " num_sets: " + num_sets)

    //
    // variables
    // 
    // The matrix
    val a = Array.fill(num_sets,n)(CPIntVar(0 to 1)(cp))


    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {
    
      for(i <- SRANGE;
          j <- SRANGE if i!=j) {
          cp.add(
                 sum(for{k <- NRANGE} yield a(i)(k)*a(j)(k)) == 0
                 )
      }

      // ensure that all integers is in
      // (exactly) one partition
      cp.add(
             sum(
                 for{i <- SRANGE
                     j <- NRANGE} yield a(i)(j)
                 ) == n
             )

      

      for(i <- SRANGE; j <- SRANGE if i < j) {
        // same cardinality
        cp.add(
               sum(for{k <- NRANGE} yield a(i)(k) ) 
               ==
               sum(for{k <- NRANGE} yield a(j)(k) )
               )

        // same sum
        cp.add(
               sum(for{k <- NRANGE} yield a(i)(k)*k ) 
               ==
               sum(for{k <- NRANGE} yield a(j)(k)*k)
               )


        // same sum squared
        cp.add(
               sum(for{k <- NRANGE} yield a(i)(k)*k*a(i)(k)*k ) 
               ==
               sum(for{k <- NRANGE} yield a(j)(k)*k*a(j)(k)*k)
               )
      }

      // symmetry breaking for num_sets == 2
      if (num_sets == 2) {
        cp.add(a(0)(0) == 1)
      }


    } search {
       
      binaryStatic(a.flatten.toSeq)
    } onSolution {
      println("\nSolution:")
      var sums = 0
      var sums_squared = 0
      for(i <- SRANGE) {
        for(j <- NRANGE if a(i)(j).value == 1) {
          print((j+1) + " ")
          if (i == 0) {
            val v = (j+1)*a(i)(j).value
            sums += v
            sums_squared += v*v
          }
        }
        println()
      }

      println("Sums: " + sums  + " Sums squared: " + sums_squared)

      numSols += 1

    }
    println(cp.start())


  }

}
