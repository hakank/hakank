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
import scala.util.Random


/**

  Stable Marriage problem in Oscar
 
  Problem and OPL model from Pascal Van Hentenryck
  "The OPL Optimization Programming Language", page 43ff.

  This version randomize the priorities.

  Also, see 
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf

  Thanks to Pierre Schaus for improving this model, both the speed and
  readability.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object StableMarriageRandom {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 5
    var num_to_show = 0 // 0: show all solutions
     
    if (args.length > 0) {
      n = args(0).toInt
    }

    if (args.length > 1) {
      num_to_show = args(1).toInt
    }

    println("n: " + n + " num_to_show: " + num_to_show)

    val Men = 0 until n
    val Women = 0 until n

    // Generate the preferences
    val ll = (1 to n).toList
    val rand = new Random(System.currentTimeMillis());
    val rankWomen = Array.fill(n)( Random.shuffle(ll).toArray )
    val rankMen   = Array.fill(n)( Random.shuffle(ll).toArray )

    println("Generated " + n + " ranks.")

    if (n <= 30) {
      for(i <- Women) {
        println("rankWomen #" + i + ": " + rankWomen(i).mkString(" "))
      }

      for(i <- Men) {
        println("rankMen   #" + i + ": " + rankMen(i).mkString(" "))
      }

      println()
    }
  
     
    //
    // decision variables
    //
    val wife    = Array.tabulate(n)(i => CPVarInt(cp, 0 to n-1))
    val husband = Array.tabulate(n)(i => CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      val t1 = System.currentTimeMillis

      for (m <- Men) {
        cp.add(husband(wife(m)) == m)
      }

      for (w <- Women) {
        cp.add(wife(husband(w)) == w)
      }      

      for (m <- Men; w <- Women) { 
        val pref_m = rankMen(m)(wife(m))      // preference of m for his wife
        val pref_w = rankWomen(w)(husband(w)) // preference of w for her husband
        
        cp.add((pref_m >>= rankMen(m)(w)) ==> (pref_w <<= rankWomen(w)(m)))
        cp.add((pref_w >>= rankWomen(w)(m)) ==> (pref_m <<= rankMen(m)(w)))         
      }
 
      val t2 = System.currentTimeMillis
      println("Constraints took " + (t2-t1) + "ms");

    } exploration {

       println("Explore...")
       
       cp.binary(wife ++ husband)

       println("wife   :" + wife.mkString(""))
       println("husband:" + husband.mkString(""))
       println()

       numSols += 1
       

      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop()
      }

    } run()

    println("\nIt was " + numSols + " solutions.\n")
    cp.printStats()

  }

}
