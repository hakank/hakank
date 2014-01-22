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
 * Map coloring in Oscar
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object MapColoring3 {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var Belgium     = 0
    val Denmark     = 1
    val France      = 2
    val Germany     = 3
    val Netherlands = 4
    val Luxembourg  = 5

    val n = 6
    val num_colors = 4

    val neighbours =  Array(Array(France,     Belgium),
                            Array(France,     Luxembourg),
                            Array(France,     Germany),
                            Array(Luxembourg, Germany),
                            Array(Luxembourg, Belgium),
                            Array(Belgium,    Netherlands),
                            Array(Belgium,    Germany),
                            Array(Germany,    Netherlands),
                            Array(Germany,    Denmark))

      
    // variables
    val color = Array.fill(n)(CPIntVar(1 to num_colors)(cp))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(i <- 0 until neighbours.length) {
        cp.add(color(neighbours(i)(0)) != color(neighbours(i)(1)))
      }

      // Symmetry breaking: Belgium has color 1
      cp.add(color(Belgium) == 1)


     } search {
       
       binaryFirstFail(color)
     
     } onSolution {
     
       println("color:" + color.mkString(" "))

       numSols += 1
       
     }
     
     println(cp.start())
   }

}
