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

  Five floors problem in Oscar
 
  From Alexey Radul & Gerald Jay Sussman: 
  "The Art of Propagator", page 34
  """
  Baker, Cooper, Fletcher, Miller, and Smith live on the first
  five floors of this apartment house. Baker does not live on the
  fifth floor. Cooper does not live on the first floor. Fletcher
  does not live on either the fifth or the first floor. Miller lives
  on a higher floor than does Cooper. Smith does not live on a
  floor adjacent to Fletcher'. Fletcher does not live on a floor
  adjacent to Cooper's.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */

object FiveFloors {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 5
       
    //
    // decision variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(baker, cooper, fletcher, miller, smith) = x

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

       cp.add(allDifferent(x), Strong)

       // Baker does not live on the fifth floor.
       cp.add(baker != 5)

       // Cooper does not live on the first floor. 
       cp.add(cooper != 1)

       // Fletcher does not live on either the fifth or the first floor. 
       cp.add(fletcher != 5)
       cp.add(fletcher != 1)

       // Miller lives on a higher floor than does Cooper. 
       cp.add(miller > cooper)

       // Smith does not live on a floor adjacent to Fletcher'. 
       cp.add((smith-fletcher).abs() > 1)

       // Fletcher does not live on a floor adjacent to Cooper's.
       cp.add((fletcher-cooper).abs() > 1)


     } exploration {
       
       cp.binaryMaxDegree(x)

       println(x.mkString(""))

       numSols += 1
       
     } run()

     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
