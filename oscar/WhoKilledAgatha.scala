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
 * Who killed agatha? (The Dreadsbury Mansion Murder Mystery)in Oscar
 *
 * This is a standard benchmark for theorem proving.
 *
 * http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
 * 
 * """ 
 * Someone in Dreadsbury Mansion killed Aunt Agatha. 
 * Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
 * are the only ones to live there. A killer always hates, and is no 
 * richer than his victim. Charles hates noone that Agatha hates. Agatha 
 * hates everybody except the butler. The butler hates everyone not richer 
 * than Aunt Agatha. The butler hates everyone whom Agatha hates. 
 * Noone hates everyone. Who killed Agatha? 
 * """
 *
 * Originally from 
 * F. J. Pelletier: Seventy-five problems for testing automatic
 *                  theorem provers.
 *                  Journal of Automated Reasoning, 2: 191–216, 1986.
 *
 * This version include some improvements suggested by Pierre Schaus.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object WhoKilledAgatha {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 3

    val NRANGE = 0 until n

    val agatha  = 0
    val butler  = 1
    val charles = 2

    val agathacp  = CPVarInt(cp, agatha)

    val names = Array("Agatha", "Butler", "Charles")

    // variables
    val the_killer = CPVarInt(cp, 0 to 2)
    val hates  = Array.fill(n,n)(CPVarBool(cp)) // who is hated by who?
    val richer = Array.fill(n,n)(CPVarBool(cp)) // who is porer than who?

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {


      //  Agatha, the butler, and Charles live in Dreadsbury Mansion, and
      //  are the only ones to live there.


      // A killer always hates, and is no richer than his victim.
      // (Seems that transpose must be used.)
      val hates_t = hates.transpose
      val richer_t = richer.transpose
      
      cp.add(hates_t(agatha)(the_killer) == 1)
      cp.add(richer_t(agatha)(the_killer) == 0)


      //  define the concept of richer: no one is richer than him-/herself
      NRANGE.foreach(i=>cp.add(!richer(i)(i)))

      //  (contd...) if i is richer than j then j is not richer than i
      for(i <- NRANGE; j <- NRANGE if i != j) {
          cp.add(richer(i)(j) != richer(j)(i))
      }

      //  Charles hates noone that Agatha hates.
      NRANGE.foreach(i=>cp.add((hates(agatha)(i)) ==> (!hates(charles)(i))))

      //  Agatha hates everybody except the butler.
      cp.add(hates(agatha)(charles))
      cp.add(hates(agatha)(agatha))
      cp.add(!hates(agatha)(butler))

      //  The butler hates everyone not richer than Aunt Agatha.
      NRANGE.foreach(i=>cp.add(!richer(i)(agatha) ==> hates(butler)(i)))

      //  The butler hates everyone whom Agatha hates.
      NRANGE.foreach(i=> cp.add(hates(agatha)(i) ==> hates(butler)(i)))

      //  Noone hates everyone.
      NRANGE.foreach(i=>cp.add(sum(hates(i)) <= 2))

      // Who killed Agatha?

   } search {
       
      binaryFirstFail(Seq(the_killer))
   } onSolution {
      println("the_killer: " + names(the_killer.value))

      numSols += 1
       
   } 

   println(cp.start())
   cp.printStats()

 }

}
