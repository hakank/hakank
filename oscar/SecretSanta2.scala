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

  Secret Santa problem II in Oscar.

  From Maple Primes: 'Secret Santa Graph Theory'
  http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
  """
  Every year my extended family does a 'secret santa' gift exchange.
  Each person draws another person at random and then gets a gift for
  them. At first, none of my siblings were married, and so the draw was
  completely random. Then, as people got married, we added the restriction
  that spouses should not draw each others names. This restriction meant
  that we moved from using slips of paper on a hat to using a simple
  computer program to choose names. Then people began to complain when
  they would get the same person two years in a row, so the program was
  modified to keep some history and avoid giving anyone a name in their
  recent history. This year, not everyone was participating, and so after
  removing names, and limiting the number of exclusions to four per person,
  I had data something like this:

  Name: Spouse, Recent Picks

  Noah: Ava. Ella, Evan, Ryan, John
  Ava: Noah, Evan, Mia, John, Ryan
  Ryan: Mia, Ella, Ava, Lily, Evan
  Mia: Ryan, Ava, Ella, Lily, Evan
  Ella: John, Lily, Evan, Mia, Ava
  John: Ella, Noah, Lily, Ryan, Ava
  Lily: Evan, John, Mia, Ava, Ella
  Evan: Lily, Mia, John, Ryan, Noah
  """

  Note: I interpret this as the following three constraints:
  1) One cannot be a Secret Santa of one's spouse
  2) One cannot be a Secret Santa for somebody two years in a row
  3) Optimization: maximize the time since the last time


  This model also handle single persons, something the original
  problem don't mention.



  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SecretSanta2 {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // Change to 1 for using with a single person
    val single = 0   

    //
    // The matrix version of earlier rounds.
    // M means that no earlier Santa has been assigned.
    // Note: Ryan and Mia has the same recipient for years 3 and 4,
    //       and Ella and John has for year 4.
    //       This seems to be caused by modification of
    //       original data.
    //
    val n_no_single = 8
    var M = n_no_single + 1
    val rounds_no_single = Array(
                                 // N  A  R  M  El J  L  Ev
                                 Array(0, M, 3, M, 1, 4, M, 2), // Noah
                                 Array(M, 0, 4, 2, M, 3, M, 1), // Ava
                                 Array(M, 2, 0, M, 1, M, 3, 4), // Ryan
                                 Array(M, 1, M, 0, 2, M, 3, 4), // Mia
                                 Array(M, 4, M, 3, 0, M, 1, 2), // Ella
                                 Array(1, 4, 3, M, M, 0, 2, M), // John
                                 Array(M, 3, M, 2, 4, 1, 0, M), // Lily
                                 Array(4, M, 3, 1, M, 2, M, 0)) // Evan
      
    //
    // Rounds with a single person (fake data)
    //
    val n_with_single = 9
    M = n_with_single + 1;
    val rounds_single = Array(
                              // N  A  R  M  El J  L  Ev S
                              Array(0, M, 3, M, 1, 4, M, 2, 2), // Noah
                              Array(M, 0, 4, 2, M, 3, M, 1, 1), // Ava
                              Array(M, 2, 0, M, 1, M, 3, 4, 4), // Ryan
                              Array(M, 1, M, 0, 2, M, 3, 4, 3), // Mia
                              Array(M, 4, M, 3, 0, M, 1, 2, M), // Ella
                              Array(1, 4, 3, M, M, 0, 2, M, M), // John
                              Array(M, 3, M, 2, 4, 1, 0, M, M), // Lily
                              Array(4, M, 3, 1, M, 2, M, 0, M), // Evan
                              Array(1, 2, 3, 4, M, 2, M, M, 0)) // Single

    val Noah   = 0
    val Ava    = 1
    val Ryan   = 2
    val Mia    = 3
    val Ella   = 4
    val John   = 5
    val Lily   = 6
    val Evan   = 7

    var n = n_no_single
    var rounds = rounds_no_single

    if (single == 1) {
      n = n_with_single
      rounds = rounds_single
    }

    M = n + 1
    val RANGE = 0 until n

    val persons = Array("Noah", "Ava", "Ryan", "Mia", "Ella",
                        "John", "Lily", "Evan", "Single")
    val spouses = Array(
                        Ava,  // Noah
                        Noah, // Ava
                        Mia,  // Rya
                        Ryan, // Mia
                        John, // Ella
                        Ella, // John
                        Evan, // Lily
                        Lily, // Evan
                        -1    // Single has no spouse
                        )


    //
    // variables
    //
    val santas = Array.fill(n)(CPIntVar(0 to n-1)(cp))
    val santa_distance = Array.fill(n)(CPIntVar(0 to M)(cp))

    // total of "distance", to maximize
    val z = sum(santa_distance)

    //
    // constraints
    //
    var numSols = 0

    cp.maximize(z) subjectTo {

      cp.add(allDifferent(santas), Strong)
      
      // Can't be one own"s Secret Santa
      // (i.e. ensure that there are no fix-point in the array.)
      for(i <- RANGE) {
        cp.add(santas(i) != i)
      }

      // no Santa for a spouses
      for(i <- RANGE if spouses(i) > -1) {
          cp.add(santas(i) != spouses(i))
      }

      // optimize "distance" to earlier rounds:
      for(i <- RANGE) {
        cp.add(santa_distance(i) == rounds(i)(santas(i)))
      }

      // cannot be a Secret Santa for the same person
      // two years in a row.
      for(i <- RANGE; j <- RANGE if rounds(i)(j) == 1) {
            cp.add(santas(i) != j)
      }


    } search {
       
      binaryFirstFail(santas ++ santa_distance)
    } onSolution {
      println("santas: " + santas.mkString(""))
      println("santa_distance: " + santa_distance.mkString(""))
      for(i <- RANGE) {
        println(persons(i) + " is a Santa to " +
                persons(santas(i).value) + " (distance " +
                santa_distance(i).value + ")") 
      }
      println()

      numSols += 1

    }

    println(cp.start())

  }

}
