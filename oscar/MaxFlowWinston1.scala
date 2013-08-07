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
 
   Max flow problem in Oscar

   From Winston 'Operations Research', page 420f, 423f
   Sunco Oil example.


   @author Hakan Kjellerstrand hakank@gmail.com
   http://www.hakank.org/oscar/
 
 */
object MaxFlowWinston1 {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 5
    val NODES = 0 until n

    // The arcs
    // Note:
    // This is 1-based to be compatible with other implementations.
    //
    val arcs1 = Array(
                      Array(1, 2),
                      Array(1, 3),
                      Array(2, 3),
                      Array(2, 4),
                      Array(3, 5),
                      Array(4, 5),
                      Array(5, 1))

    val arc_len = arcs1.length
    val ARCS = 0 until arc_len

    // Capacities
    val cap = Array(2,3,3,4,2,1,100)

    // Convert arcs1 to 0-based (arcs)
    val arcs = Array.tabulate(arc_len)(i=> Array.tabulate(2)(j=> arcs1(i)(j) - 1))

    // Convert arcs to matrix (for checking below)
    val mat  = Array.fill(arc_len)(Array.fill(arc_len)(0)) 
    for(i <- NODES) {
      for(j <- NODES) {
        var c = 0
        for(k <- ARCS if arcs(k)(0) == i && arcs(k)(1) == j) {
          c = 1
        }
        mat(i)(j) = c
      }
      println(mat(i).mkString(" "))
    }


    // variables
    val flow = Array.fill(n,n)(CPVarInt(cp, 0 to 200))
    // to maximize
    val z = flow(n-1)(0)


    //
    // constraints
    //
    var numSols = 0

    cp.maximize(z) subjectTo {

      // capacity of arcs
      for(i <- ARCS) {
        cp.add(flow(arcs(i)(0))(arcs(i)(1)) <= cap(i))
      }

      // inflows == outflows
      for(i <- NODES) {
        val s1 = sum(for(k <- ARCS if arcs(k)(1) == i)
                       yield flow(arcs(k)(0))(arcs(k)(1)))
                       
        val s2 = sum(for(k <- ARCS if arcs(k)(0) == i)
                       yield flow(arcs(k)(0))(arcs(k)(1)))
        
        cp.add(s1 == s2)
        
      }
      
      // Just arcs with connections can have a flow.
      for(i <- NODES; j <- NODES) {
        if (mat(i)(j) == 0) {
          cp.add(flow(i)(j) == 0)
        }
      }


    } exploration {
       
      cp.binary(flow.flatten)

      println("z: " + z)
      for(i <- NODES) {
        for(j <- NODES) {
          print("%3d".format(flow(i)(j).value))
        }
        println()
      }
      println()

      numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
