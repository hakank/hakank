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

  Photo problem in Oscar.
  
  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one
  row for taking a photo. Some of them have preferences next to whom
  they want to stand:
  
    1. Betty wants to stand next to Gary and Mary.
    2. Chris wants to stand next to Betty and Gary.
    3. Fred wants to stand next to Mary and Donald.
    4. Paul wants to stand next to Fred and Donald.
   
  Obviously, it is impossible to satisfy all preferences. Can you find
  an alignment that maximizes the number of satisfied preferences?
  """

  Oz solution:
  6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]


  4 solutions for solveAll (optimal z = 6)
  Here are the places of each person:
    Donald Paul Fred Mary Betty Chris Gary
    Paul Donald Fred Mary Betty Chris Gary
    Donald Paul Fred Mary Betty Gary Chris
    Paul Donald Fred Mary Betty Gary Chris



  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object PhotoProblem {


  // 
  // Decomposition of inverse constraint
  // 
  // Channel of positions of x and y:
  //    j == x(i) <=> y(j) == i
  // 
  // Here: 
  //   x is the position array
  //   y are the placements
  // 
  def inverse(cp: CPSolver, x: Array[CPIntVar], y: Array[CPIntVar]) {
    val len = x.length
      for(i <- 0 until len;
          j <- 0 until len) {
        cp.add( (y(j) === i) == (x(i) === j) )
      }
  }

    

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    val preferences = Array(
                                 // 0 1 2 3 4 5 6
                                 // B C D F G M P
                              Array(0,0,0,0,1,1,0), // Betty  0
                              Array(1,0,0,0,1,0,0), // Chris  1
                              Array(0,0,0,0,0,0,0), // Donald 2
                              Array(0,0,1,0,0,1,0), // Fred   3
                              Array(0,0,0,0,0,0,0), // Gary   4
                              Array(0,0,0,0,0,0,0), // Mary   5
                              Array(0,0,1,1,0,0,0)) // Paul   6

    val persons = Array("Betty", "Chris", "Donald", "Fred", "Gary", "Mary", "Paul")
    val n = preferences.length

    println("Preferences:");
    println("1. Betty wants to stand next to Gary and Mary.");
    println("2. Chris wants to stand next to Betty and Gary.");
    println("3. Fred wants to stand next to Mary and Donald.");
    println("4. Paul wants to stand next to Fred and Donald.\n");

    //
    // variables
    //
    val positions = Array.fill(n)(CPIntVar(0 to n-1)(cp))
    val places    = Array.fill(n)(CPIntVar(0 to n-1)(cp))

    // calculate all the successful preferences
    val z =  sum(
                 for{i <- 0 until n
                     j <- 0 until n
                     if preferences(i)(j) == 1
                 } yield ((positions(i)-positions(j)).abs() === 1)
                 )
      

    //
    // constraints
    //
    var numSols = 0

    cp.maximize(z) subjectTo {
    // cp.solveAll subjectTo {

      cp.add(allDifferent(positions), Strong)
      cp.add(allDifferent(places), Strong)

      // Symmetry breaking (from the Oz page):
      //    Fred is somewhere left of Betty
      cp.add(positions(3) < positions(0));

      // Channeling positions <=> places
      inverse(cp, positions, places)

      // for solveAll
      // cp.add(z == 6)


    } search {
       
      binaryStatic(positions)
    } onSolution {
      println("\nSolution:")
      println("positions: " + positions.mkString(""))
      println("places   : " + places.mkString(""))
      println("places   : " + places.map(p=>persons(p.value)).mkString(" "))
      println("Successful preferences:")
      for(i <- 0 until n;
          j <- 0 until n 
            if 
            preferences(i)(j) == 1 && 
            abs(positions(i).value-positions(j).value) == 1
          ) {
        println(persons(i) + ": " + persons(j))
      }

      println("\nUnsuccessful preferences:")
      for(i <- 0 until n;
          j <- 0 until n 
            if 
            preferences(i)(j) == 1 && 
            abs(positions(i).value-positions(j).value) > 1
          ) {
        println(persons(i) + ": " + persons(j))
      }
      println()

      numSols += 1

   } 
   println(cp.start())
  }

}
