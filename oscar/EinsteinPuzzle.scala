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

  Einstein puzzle in Oscar.

  This is a variant of the Zebra puzzle.
  

  See: 
  - http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html
  - http://en.wikipedia.org/wiki/Zebra_Puzzle#Other_versions

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object EinsteinPuzzle {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    val RANGE = 0 until n
    val Array(brit, swede, dane, norwegian, german)            = RANGE.toArray
    val Array(dogs, fish, birds, cats, horses)                 = RANGE.toArray
    val Array(tea, coffee, milk, beer, water)                  = RANGE.toArray
    val Array(pall_mall, dunhill, blends, blue_master, prince) = RANGE.toArray
    val Array(red, green, white, yellow, blue)                 = RANGE.toArray


    //
    // variables
    // 
    val nationality = Array.fill(n)(CPIntVar(RANGE)(cp))
    val animal      = Array.fill(n)(CPIntVar(RANGE)(cp))
    val drink       = Array.fill(n)(CPIntVar(RANGE)(cp))
    val smoke       = Array.fill(n)(CPIntVar(RANGE)(cp))
    val color       = Array.fill(n)(CPIntVar(RANGE)(cp))

    //
    // constraints
    //
    cp.solve subjectTo {

       cp.add(allDifferent(nationality), Strong)    
       cp.add(allDifferent(animal)     , Strong)
       cp.add(allDifferent(drink)      , Strong)
       cp.add(allDifferent(smoke)      , Strong)
       cp.add(allDifferent(color)      , Strong)

       //
       // The clues
       //

       // 1. The Brit lives in a red house.
       cp.add(nationality(brit) == color(red))

       // 2. The Swede keeps dogs as pets.
       cp.add(nationality(swede) == animal(dogs))

       // 3. The Dane drinks tea.
       cp.add(nationality(dane) == drink(tea))

       // 4. The Green house is just on the left of the White house.
       cp.add(color(green) == color(white)-1)

       // 5. The owner of the Green house drinks coffee.
       cp.add(color(green) == drink(coffee))

       // 6. The person who smokes Pall Mall keeps birds.
       cp.add(smoke(pall_mall) == animal(birds))

       // 7. The owner of the Yellow house smokes Dunhill.
       cp.add(color(yellow) == smoke(dunhill))

       // 8. The man living in the center house drinks milk.
       cp.add(drink(milk) == 2)

       // 9. The Norwegian lives in the first house.
       cp.add(nationality(norwegian) == 0)

       // 10. The man who smokes Blends lives next to the one who keeps cats.
       cp.add((smoke(blends) - animal(cats)).abs == 1)

       // 11. The man who smokes Blue Master drinks beer.
       cp.add(smoke(blue_master) == drink(beer))

       // 12. The man who keeps horses lives next to the man who smokes Dunhill.
       cp.add((animal(horses) - smoke(dunhill)).abs == 1)

       // 13. The German smokes Prince.
       cp.add(nationality(german) == smoke(prince))

       // 14. The Norwegian lives next to the blue house.
       cp.add((nationality(norwegian) - color(blue)).abs == 1)

       // 15. The man who smokes Blends has a neighbour who drinks water.
       cp.add((smoke(blends) - drink(water)).abs == 1);      


    } search {
       
      binaryFirstFail(nationality ++ animal ++ drink ++ smoke ++ color)
      
    } onSolution {
      
      println("\nSolution:")
      val nats = "Brit, Swede, Dane, Norwegian, German"            split(", *")
      val anis = "dogs, fish, birds, cats, horses"                 split(", *")
      val dris = "tea, coffee, milk, beer, water"                  split(", *")
      val smos = "Pall Mall, Dunhill, Blends, Blue Master, Prince" split(", *")
      val cols = "red, green, white, yellow, blue"                 split(", *")

      // find the index in x where thing is
      def getIndex(x : Array[CPIntVar], thing: Int) =
        x.zipWithIndex.filter(_._1.value == thing)(0)._2 


      println((for(house <- RANGE) 
                 yield ("House " + house                   + ": " + 
                        nats(getIndex(nationality, house)) + " "  +
                        anis(getIndex(animal     , house)) + " "  +
                        dris(getIndex(drink      , house)) + " "  +
                        smos(getIndex(smoke      , house)) + " "  +
                        cols(getIndex(color      , house)))
               ).mkString("\n"))

      println("\nWho owns the fish: The " + nats(getIndex(animal, fish )))

    }
    
    println(cp.start())


  }

}
