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

  Zebra problem in Oscar.

  """
  This is the zebra problem as invented by Lewis Caroll.

  There are five houses.
  The Englishman lives in the red house.
  The Spaniard owns the dog.
  Coffee is drunk in the green house.
  The Ukrainian drinks tea.
  The green house is immediately to the right of the ivory house.
  The Old Gold smoker owns snails.
  Kools are smoked in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian lives in the first house.
  The man who smokes Chesterfields lives in the house next to the man
    with the fox.
  Kools are smoked in the house next to the house where the horse is kept.
  The Lucky Strike smoker drinks orange juice.
  The Japanese smokes Parliaments.
  The Norwegian lives next to the blue house.

  Who owns a zebra and who drinks water?
  """
   
  Note: A different (and simpler) approach is shown in Zebra2.scala.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Zebra {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    //
    // variables
    // 
    // Colors
    val red           = CPIntVar(1 to n)(cp)
    val green         = CPIntVar(1 to n)(cp)
    val yellow        = CPIntVar(1 to n)(cp)
    val blue          = CPIntVar(1 to n)(cp)
    val ivory         = CPIntVar(1 to n)(cp)

    // Nationality
    val englishman    = CPIntVar(1 to n)(cp)
    val spaniard      = CPIntVar(1 to n)(cp)
    val japanese      = CPIntVar(1 to n)(cp)
    val ukrainian     = CPIntVar(1 to n)(cp)
    val norwegian     = CPIntVar(1 to n)(cp)

    // Animal
    val dog           = CPIntVar(1 to n)(cp)
    val snails        = CPIntVar(1 to n)(cp)
    val fox           = CPIntVar(1 to n)(cp)
    val zebra         = CPIntVar(1 to n)(cp)
    val horse         = CPIntVar(1 to n)(cp)

    // Drink
    val tea           = CPIntVar(1 to n)(cp)
    val coffee        = CPIntVar(1 to n)(cp)
    val water         = CPIntVar(1 to n)(cp)
    val milk          = CPIntVar(1 to n)(cp)
    val fruit_juice   = CPIntVar(1 to n)(cp)

    // Smoke
    val old_gold      = CPIntVar(1 to n)(cp)
    val kools         = CPIntVar(1 to n)(cp)
    val chesterfields = CPIntVar(1 to n)(cp)
    val lucky_strike  = CPIntVar(1 to n)(cp)
    val parliaments   = CPIntVar(1 to n)(cp)

    // for labeling
    val all_vars = Array(
                         parliaments, kools, chesterfields, lucky_strike, old_gold,
                         englishman, spaniard, japanese, ukrainian, norwegian,
                         dog, snails, fox, zebra, horse,
                         tea, coffee, water, milk, fruit_juice,
                         red, green, yellow, blue, ivory)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {
    
       cp.add(allDifferent(Array(red, green, yellow, blue, ivory)), Strong)
       cp.add(allDifferent(Array(englishman, spaniard, japanese, ukrainian, norwegian)), Strong)
       cp.add(allDifferent(Array(dog, snails, fox, zebra, horse)),Strong)
       cp.add(allDifferent(Array(tea, coffee, water, milk, fruit_juice)), Strong)
       cp.add(allDifferent(Array(parliaments, kools, chesterfields, lucky_strike, old_gold)), Strong)


       //
       // The clues
       //
       cp.add(englishman == red)
       cp.add(spaniard == dog)
       cp.add(coffee == green)
       cp.add(ukrainian == tea)
       cp.add(green == ivory + 1)
       cp.add(old_gold == snails)
       cp.add(kools == yellow)
       cp.add(milk == 3)
       cp.add(norwegian == 1)
       cp.add((fox - chesterfields).abs() == 1)
       cp.add((horse - kools).abs() == 1)
       cp.add(lucky_strike == fruit_juice)
       cp.add(japanese == parliaments)
       cp.add((norwegian - blue).abs() == 1)



    } search {
       
      binaryStatic(all_vars)
    } onSolution {
      println("\nSolution:")
      val p  = Array(englishman, spaniard, japanese, ukrainian, norwegian)
      val ps = Array("englishman", "spaniard", "japanese", "ukrainian", "norwegian")

      println("water drinker: " + ps((for{i <- 0 until n if p(i).value == water.value} yield i).head))
      println("owns zebra: " + ps((for{i <- 0 until n if p(i).value == zebra.value} yield i).head))

      numSols += 1

    } 
    println(cp.start())

  }

}
