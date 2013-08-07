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

  Four islands problem (Dell Logic Puzzles) in Oscar.

  http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object FourIslands {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 4

    //
    // variables
    //
    // the digits
    val island = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(pwana, quero, rayou, skern) = island
    val islandStr = Array("Pwana", "Quero", "Rayou", "Skern")

    val export = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(alabaster, bananas, coconuts, durian_fruit) = export
    val exportStr = Array("alabaster", "bananas", "coconuts", "durian fruit")

    val attraction =  Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(resort_hotel, ice_skating_rink, jai_alai_stadium, koala_preserve) = attraction
    val attractionStr = Array("resort hotel", "ice skating rink", "jai alai stadium", "koala preserve")
    val Array(a,b,c,d) = (0 to n-1).toArray

    val max_len = Array(islandStr, exportStr, attractionStr).flatten.map(_.length).max

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {
      
      cp.add(allDifferent(island), Strong)
      cp.add(allDifferent(export), Strong)
      cp.add(allDifferent(attraction), Strong)
      
      //  1. The island noted for its koala preserve is due south of Pwana.
      cp.add(
       (pwana === a && koala_preserve === c)
       ||
       (pwana === b && koala_preserve === d)
      ) 

      //  2. The island with the largest alabaster quarry is due west of Quero.
      cp.add( 
       (alabaster === a && quero === b) 
       || 
       (alabaster === c && quero === d) 
      )

      //  3. The island with the resort hotel is due east of the one that exports 
      //     durian fruit.
      cp.add( 
       ( durian_fruit === a && resort_hotel ===  b )
       ||
       ( durian_fruit === c && resort_hotel ===  d)
      )
      
      //  4. Skern and the island with the jai alai stadium are connected by a 
      //     north-south bridge. 
      cp.add(
       (skern === a && jai_alai_stadium === c) 
       ||
       (skern === c && jai_alai_stadium === a) 
       ||
       (skern === b && jai_alai_stadium === d) 
       ||
       (skern === d && jai_alai_stadium === b) 
      ) 
      
      //  5. Rayou and the island that exports bananas are connected by an 
      //     east-west bridge.
      cp.add(
       (rayou === a && bananas === b) 
       ||
       (rayou === b && bananas === a) 
       ||
       (rayou === c && bananas === d) 
       ||
       (rayou === d && bananas === c) 
      )
      
      //  6. The islands noted for the South Pacific's largest ice skating rink 
      //     and for the jai alai stadium are not connected by a bridge.
      cp.add( 
       (ice_skating_rink === a && jai_alai_stadium === d)
       ||
       (ice_skating_rink === d && jai_alai_stadium === a)
       ||
       (ice_skating_rink === b && jai_alai_stadium === c)
       ||
       (ice_skating_rink === c && jai_alai_stadium === b)
      )

    } exploration {
     
      cp.binary(island)

      println("\nSolution:")
      println("island    : " +  island.mkString(""))
      println("export    : " +  export.mkString(""))
      println("attraction: " +  attraction.mkString(""))
      println()

      // Nicer output      
      val f = "%-"+(max_len+1)+"s"
      println(Array("A","B","C","D").map(i=>f.format(i)).mkString(" "))
      println(island.map(i=>f.format(islandStr(i.value))).mkString(" "))
      println(export.map(e=>f.format(exportStr(e.value))).mkString(" "))
      println(attraction.map(a=>f.format(attractionStr(a.value))).mkString(" "))

      println()

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
