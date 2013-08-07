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

  Crypto in Oscar.

  Standard alphametic problem in mathematical recreations, 
  constraint programming etc.

  This is an alternative approach compared to Crypto.scala.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Crypto2 {


  // sums(all, "ballet", 45, ht)
  def sums(x: Array[CPVarInt], str: String, s: Int, m: Map[Char,Int]) =
    sum( for(v <- str.toCharArray.map(m(_))) yield(x(v)) ) === s


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num = 26

    val ht = ('a' to 'z').zipWithIndex.toMap // char -> index

    //
    // variables
    //
    val all = Array.fill(num)(CPVarInt(cp, 1 to num))
    val Array(a,b,c,d,e,f,g,h,i,j,k,l,m) = all slice( 0, 13)
    val Array(n,o,p,q,r,s,t,u,v,w,x,y,z) = all slice(13, 26)


    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(all), Strong)

      cp.add(sums(all, "ballet",     45, ht))
      cp.add(sums(all, "cello",      43, ht))
      cp.add(sums(all, "concert",    74, ht))
      cp.add(sums(all, "flute",      30, ht))
      cp.add(sums(all, "fugue",      50, ht))
      cp.add(sums(all, "glee",       66, ht))
      cp.add(sums(all, "jazz",       58, ht))
      cp.add(sums(all, "lyre",       47, ht))
      cp.add(sums(all, "oboe",       53, ht))
      cp.add(sums(all, "opera",      65, ht))
      cp.add(sums(all, "polka",      59, ht))
      cp.add(sums(all, "quartet",    50, ht))
      cp.add(sums(all, "saxophone", 134, ht))
      cp.add(sums(all, "scale",      51, ht))
      cp.add(sums(all, "solo",       37, ht))
      cp.add(sums(all, "song",       61, ht))
      cp.add(sums(all, "soprano",    82, ht))
      cp.add(sums(all, "theme",      72, ht))
      cp.add(sums(all, "violin",    100, ht))
      cp.add(sums(all, "waltz",      34, ht))

      
    } exploration {
       
      cp.binaryMaxDegree(all)

      println("all:" + all.mkString(""))

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
