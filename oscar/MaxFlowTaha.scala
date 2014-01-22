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

   From Taha "Introduction to Operations Research", Example 6.4-2

   Translated from the AMPL code at
   http://taha.ineg.uark.edu/maxflo.txt

   
   @author Hakan Kjellerstrand hakank@gmail.com
   http://www.hakank.org/oscar/
 
 */
object MaxFlowTaha {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n     = 5;
    val start = 0;
    val end   = n-1;

    val NODES = 0 until n

    // cost matrix
    val c = Array(Array(0, 20, 30, 10,  0),
                  Array(0,  0, 40,  0, 30),
                  Array(0,  0,  0, 10, 20),
                  Array(0,  0,  5,  0, 20),
                  Array(0,  0,  0,  0,  0))

      
    // variables
    val x = Array.tabulate(n)(i=>
                              Array.tabulate(n)(j =>
                                                CPIntVar(0 to c(i)(j))(cp)))

    val out_flow = Array.fill(n)(CPIntVar(0 to 1000)(cp))
    val in_flow = Array.fill(n)(CPIntVar(0 to 1000)(cp))
    val total = sum(for{j <- NODES
                      if c(start)(j) > 0} yield x(start)(j))


    //
    // constraints
    //
    var numSols = 0

    cp.maximize(total) subjectTo {

      for(i <- NODES) {
        val in_flow_sum = for{j <- NODES if c(j)(i) > 0} yield x(j)(i)
        if (in_flow_sum.length > 0) {
          cp.add(sum(in_flow_sum)  == in_flow(i))
        }

        val out_flow_sum = for(j <- NODES if c(i)(j) > 0) yield x(i)(j)
        if (out_flow_sum.length > 0) {
          cp.add(sum(out_flow_sum)  == out_flow(i))
        }
        
      }
      
      // in_flow == out_flow
      for(i <- NODES if i != start && i != end) {
        cp.add(out_flow(i) == in_flow(i))
      }
      
      val s1 = for(i <- NODES if c(i)(start) > 0) yield x(i)(start)
      if (s1.length > 0) {
        cp.add(sum(s1) == 0)
      }
      
      val s2 = for(j <- NODES if c(end)(j) > 0) yield x(end)(j)
      if (s2.length > 0) {
        cp.add(sum(s2) == 0)
      }


     } search {
       
      binary(x.flatten.toSeq, _.min, _.max)
     
     } onSolution {
      println("total: " + total)
      println(x.map(i=>i.map(j=>"%3d".format(j.value)).mkString(" ")).mkString("\n"))

      println()

      numSols += 1
       
     }
     println(cp.start())

   }

}
