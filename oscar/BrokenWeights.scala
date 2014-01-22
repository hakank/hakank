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

  Broken weight problem in Oscar.

  From http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Maziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrieâ€™s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object BrokenWeights {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var m = 40
    var n = 4

    if (args.length > 0) {
      m = args(0).toInt
    }

    if (args.length > 1) {
      n = args(1).toInt
    }


    //
    // variables
    //
    val weights = Array.fill(n)(CPIntVar(1 to m)(cp))
    val x   = Array.fill(m, n)(CPIntVar(-1 to 1)(cp))
    val x_flat = x.flatten

    //
    // constraints
    //

    cp.minimize(weights(n-1)) subjectTo {

      // total weight of the pieces
      cp.add(sum(weights) == m)

      // ensure that all weighst are handled
      for(i <- 0 until m) {
        cp.add(sum(for(j <- 0 until n) yield x(i)(j)*weights(j)) == i+1)
      }

      // symmetry breaking
      for(j <- 1 until n) {
        cp.add(weights(j-1) < weights(j))
      }

      
    } search {
       
      binaryMaxDegree(weights ++ x_flat)

    } onSolution {
      
      println("\nSolution:")
      println("weights:" + weights.mkString(""))
      for(i <- 0 until m) {
        print("weight " + "%2s".format(i+1) + ": ")
        for(j <- 0 until n) {        
          print("%3s".format(x(i)(j).value) + " ")
        }
        println()
      }
    }
    
    println(cp.start())

  }

}
