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

  Secret Santa problem in Oscar.
  
  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  """
  Honoring a long standing tradition started by my wife's dad, my friends
  all play a Secret Santa game around Christmas time. We draw names and
  spend a week sneaking that person gifts and clues to our identity. On the
  last night of the game, we get together, have dinner, share stories, and,
  most importantly, try to guess who our Secret Santa was. It's a crazily
  fun way to enjoy each other's company during the holidays.
   *
  To choose Santas, we use to draw names out of a hat. This system was
  tedious, prone to many 'Wait, I got myself...' problems. This year, we
  made a change to the rules that further complicated picking and we knew
  the hat draw would not stand up to the challenge. Naturally, to solve
  this problem, I scripted the process. Since that turned out to be more
  interesting than I had expected, I decided to share.
   *
  This weeks Ruby Quiz is to implement a Secret Santa selection script.
  *  Your script will be fed a list of names on STDIN.
  ...
  Your script should then choose a Secret Santa for every name in the list.
  Obviously, a person cannot be their own Secret Santa. In addition, my friends
  no longer allow people in the same family to be Santas for each other and your
  script should take this into account.
  """

  Comment: This model skips the file input and mail parts. We
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families.

  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SecretSanta {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    val num_to_show = if (args.length > 0) args(0).toInt else 1;

    val family = Array(1,1,1,1, 2, 3,3,3,3,3, 4,4)
    val n = family.length

    //
    // variables
    //
    val x = Array.fill(n)(CPIntVar(0 to n-1)(cp))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(x), Strong)

      // Can't be one own's Secret Santa
      // (i.e. ensure that there are no fix-point in x)
      for(i <- 0 until n) {
        cp.add(x(i) != i)
      }

      // No Secret Santa to a person in the same family
      for(i <- 0 until n) {
        cp.add(family(x(i)) != family(i))
      }
      
    } search {
       
      binaryStatic(x)
    } onSolution {
      println("\nSolution:")

      println("x: " + x.mkString(""))
      for(i <- 0 until n) {
        println("Person " + i + " (family " + family(i) + ") is a Secret Santa of" + x(i) + " (family " + family(x(i).value) + ")")
      }

      numSols += 1

   } 
   println(cp.start(nSols = num_to_show))

  }

}
