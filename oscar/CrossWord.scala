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
import oscar.cp.constraints._
import collection.mutable._
import scala.collection.JavaConversions._

/**

  Crosswords in OscaR.

  This is a standard example for constraint logic programming. See e.g.

  http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
  '''
  We are to complete the puzzle

     1   2   3   4   5
   +---+---+---+---+---+       Given the list of words:
 1 | 1 |   | 2 |   | 3 |             AFT     LASER
   +---+---+---+---+---+             ALE     LEE
 2 | # | # |   | # |   |             EEL     LINE
   +---+---+---+---+---+             HEEL    SAILS
 3 | # | 4 |   | 5 |   |             HIKE    SHEET
   +---+---+---+---+---+             HOSES   STEER
 4 | 6 | # | 7 |   |   |             KEEL    TIE
   +---+---+---+---+---+             KNOT
 5 | 8 |   |   |   |   |
   +---+---+---+---+---+
 6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
   +---+---+---+---+---+       puzzle correspond to the words
                               that will start at those locations.
  '''

  The model was inspired by Sebastian Brand's Array Constraint cross word example
  http://www.cs.mu.oz.au/~sbrand/project/ac/
  http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl



  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object CrossWord {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    // 
    val alpha = Array(" ","a","b","c","d","e","f",
                      "g","h","i","j","k","l","m",
                      "n","o","p","q","r","s","t",
                      "u","v","w","x","y","z")

    val a=1;  val b=2;  val c=3; val d=4;  val e=5;  val f=6;
    val g=7;  val h=8;  val i=9; val j=10; val k=11; val l=12;
    val m=13; val n=14; val o=15; val p=16; val q=17; val r=18;
    val s=19; val t=20; val u=21; val v=22; val w=23; val x=24;
    val y=25; val z=26;

    val num_words = 15
    val word_len = 5

    val AA = Array(Array(h, o, s, e, s),  //  HOSES
                   Array(l, a, s, e, r),  //  LASER
                   Array(s, a, i, l, s),  //  SAILS
                   Array(s, h, e, e, t),  //  SHEET
                   Array(s, t, e, e, r),  //  STEER
                   Array(h, e, e, l, 0),  //  HEEL
                   Array(h, i, k, e, 0),  //  HIKE
                   Array(k, e, e, l, 0),  //  KEEL
                   Array(k, n, o, t, 0),  //  KNOT
                   Array(l, i, n, e, 0),  //  LINE
                   Array(a, f, t, 0, 0),  //  AFT
                   Array(a, l, e, 0, 0),  //  ALE
                   Array(e, e, l, 0, 0),  //  EEL
                   Array(l, e, e, 0, 0),  //  LEE
                   Array(t, i, e, 0, 0))  //  TIE

    val num_overlapping = 12
    val overlapping = Array(Array(0, 2, 1, 0),  //  s
                            Array(0, 4, 2, 0),  //  s
                            
                            Array(3, 1, 1, 2),  //  i
                            Array(3, 2, 4, 0),  //  k
                            Array(3, 3, 2, 2),  //  e
                            
                            Array(6, 0, 1, 3),  //  l
                            Array(6, 1, 4, 1),  //  e
                            Array(6, 2, 2, 3),  //  e
                            
                            Array(7, 0, 5, 1),  //  l
                            Array(7, 2, 1, 4),  //  s
                            Array(7, 3, 4, 2),  //  e
                            Array(7, 4, 2, 4))  //  r

    val N = 8

    //
    // variables
    //
    val A = Array.fill(num_words,word_len)(CPIntVar(0 to 26)(cp))
    val A_flatten = A.flatten

    val E = Array.fill(N)(CPIntVar(0 to num_words)(cp))
    
    //
    // constraints
    //

    cp.solve subjectTo {

      cp.add(allDifferent(E), Strong)

      for(I <- 0 until num_words;
          J <- 0 until word_len) {
          cp.add(A(I)(J) == AA(I)(J));
      }


      // Handle the overlappings.
      //
      // It's coded in MiniZinc as:
      //   forall(i in 1..num_overlapping) (
      //      A[E[overlapping[i,1]], overlapping[i,2]] =
      //      A[E[overlapping[i,3]], overlapping[i,4]]
      //   )
      // 
      for(I <- 0 until num_overlapping) {
           cp.add(
                  A_flatten(E(overlapping(I)(0))*word_len+overlapping(I)(1))
                  ==
                  A_flatten(E(overlapping(I)(2))*word_len+overlapping(I)(3)),
                  Strong
                  )
      }


     } search {

        binaryStatic(E)
        
     } onSolution {
       
        println("E: " + E.mkString(" "))
        for(ee <- 0 until N) {
          print(ee + ": (" + "%2d".format(E(ee).value) + ") ")
          for(ii <- 0 until word_len) {
            print(alpha(A(ee)(ii).value))
          }
          println()
        }
        println()


     } 
     
     println(cp.start())
   }

}
