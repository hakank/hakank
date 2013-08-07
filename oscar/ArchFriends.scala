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

  Arch friends puzzle (Dell Logic Puzzles) in Oscar.

  Problem formulation from 
  http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four shoe 
  purchases to her friend Aurora. Aurora just loves the four different kinds 
  of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
  and suede sandals), but Harriet can't recall at which different store (Foot 
  Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
  Can you help these two figure out the order in which Harriet bought each 
  pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
  
  Determine: Order - Shoes - CPStore 
  ""

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object ArchFriends {

   // 
   // Decomposition of inverse constraint
   // 
   // Channel of positions of x and y:
   //    j == x(i) <=> y(j) == i
   // 
   // Note: This requires the domain 0..n-1
   //
   def inverse(cp: CPSolver, x: Array[CPVarInt], y: Array[CPVarInt]) {
      val len = x.length
      for(i <- 0 until len;
          j <- 0 until len) {
        cp.add( (y(j) === i) == (x(i) === j) )
      }
   }

   // Convenient function which returns y (for presentation)
   def inverse2(cp: CPSolver, x: Array[CPVarInt]) : Array[CPVarInt] = {
     val y = Array.fill(x.length)(CPVarInt(cp, x(0).min to x(0).max))
     inverse(cp, x, y)
     y
   }


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 4

    //
    // variables
    //
    val shoes = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(ecru_espadrilles, fuchsia_flats, purple_pumps, suede_sandals) = shoes
    // for output
    val shoesStr = Array("Ecru Espadrilles", "Fuchsia Flats", "Purple Pumps", "Suede Sandals")
    val shoesInv = inverse2(cp, shoes)

    val shops = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(foot_farm, heels_in_a_handcart, the_shoe_palace, tootsies) = shops
    // for output
    val shopsStr = Array("Foot Farm", "Heels in a Handcart", "The Shoe Palace", "Tootsies")
    val shopsInv = inverse2(cp, shops)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(shoes), Strong)
      cp.add(allDifferent(shops), Strong)

      // 1. Harriet bought fuchsia flats at Heels in a Handcart.
      cp.add(fuchsia_flats == heels_in_a_handcart)

      // 2. The store she visited just after buying her purple
      //    pumps was not Tootsies.
      cp.add(purple_pumps + 1 != tootsies)

      //  3. The Foot Farm was Harriet's second stop.
      cp.add(foot_farm == 1)

      // 4. Two stops after leaving The Shoe Place, Harriet 
      //    bought her suede sandals.
      cp.add(the_shoe_palace + 2 == suede_sandals)


    } exploration {
       
      cp.binaryMaxDegree(shoes ++ shops)

      println("Shops: " + shops.mkString(" "))
      println("Shoes: " + shoes.mkString(" "))
      println()
      println("Shops: " + shopsInv.map(s=>shopsStr(s.value)).mkString(", "))
      println("Shoes: " + shoesInv.map(s=>shoesStr(s.value)).mkString(", "))
      println()
      println((0 until n).
              map(s=>Array(shopsStr(shopsInv(s).value), shoesStr(shoesInv(s).value)).mkString(": ")).mkString("\n"))

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
