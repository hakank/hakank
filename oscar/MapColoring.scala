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
object MapColoring {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val num_colors = 4

    // Belgium, Denmark, France, Germany, Netherlands, Luxembourg
    val connections = Array(Array(0, 0, 1, 1, 1, 1),
                            Array(0, 0, 0, 1, 0, 0),
                            Array(1, 0, 0, 1, 1, 0),
                            Array(1, 1, 1, 0, 1, 1),
                            Array(1, 0, 1, 1, 0, 0),
                            Array(1, 0, 0, 1, 0, 0))

    val num_countries = connections.length
      
    // variables
    val color = Array.fill(num_countries)(CPIntVar(1 to num_colors)(cp))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for (c1 <- 0 until num_countries; 
           c2 <- 0 until c1 if connections(c1)(c2)==1) {
          cp.add(color(c1) != color(c2))
      }

      // Symmetry breaking: Belgium has color 1
      cp.add(color(0) == 1)


     } search {
       
       binaryFirstFail(color)
     } onSolution {
       
       println("color:" + color.mkString(""))

       numSols += 1
       
     }
     println(cp.start())
   }

}
