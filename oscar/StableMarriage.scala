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

  Stable Marriage problem in Oscar
 
  Problem and OPL model from Pascal Van Hentenryck
  "The OPL Optimization Programming Language", page 43ff.

  Also, see 
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf

  Note: There is a better (and nicer) implementation of the constraints in
        Pierre Schaus' improved version:
          oscar/examples/cp/StableMariage.scala
        which I copied to my own variant (randomizing the preferences)
          StableMarriageRandom.scala


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object StableMarriage {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data

    //
    // From Pascal Van Hentenryck's OPL book
    //
    val van_hentenryck = 
      // rankWomen
      Array(
           Array(
                Array(1, 2, 4, 3, 5),
                Array(3, 5, 1, 2, 4),
                Array(5, 4, 2, 1, 3),
                Array(1, 3, 5, 4, 2),
                Array(4, 2, 3, 5, 1)),
           
           // rankMen
           Array(
                Array(5, 1, 2, 4, 3),
                Array(4, 1, 3, 2, 5),
                Array(5, 3, 2, 4, 1),
                Array(1, 5, 4, 3, 2),
                Array(4, 3, 2, 1, 5))
           )


    //
    // Data from MathWorld
    // http://mathworld.wolfram.com/StableMarriageProblem.html
    //
    val mathworld = Array(
                          // rankWomen
                          Array(
                                Array(3, 1, 5, 2, 8, 7, 6, 9, 4),
                                Array(9, 4, 8, 1, 7, 6, 3, 2, 5),
                                Array(3, 1, 8, 9, 5, 4, 2, 6, 7),
                                Array(8, 7, 5, 3, 2, 6, 4, 9, 1),
                                Array(6, 9, 2, 5, 1, 4, 7, 3, 8),
                                Array(2, 4, 5, 1, 6, 8, 3, 9, 7),
                                Array(9, 3, 8, 2, 7, 5, 4, 6, 1),
                                Array(6, 3, 2, 1, 8, 4, 5, 9, 7),
                                Array(8, 2, 6, 4, 9, 1, 3, 7, 5)),
                          
                          // rankMen
                          Array(
                                Array(7, 3, 8, 9, 6, 4, 2, 1, 5),
                                Array(5, 4, 8, 3, 1, 2, 6, 7, 9),
                                Array(4, 8, 3, 9, 7, 5, 6, 1, 2),
                                Array(9, 7, 4, 2, 5, 8, 3, 1, 6),
                                Array(2, 6, 4, 9, 8, 7, 5, 1, 3),
                                Array(2, 7, 8, 6, 5, 3, 4, 1, 9),
                                Array(1, 6, 2, 3, 8, 5, 4, 9, 7),
                                Array(5, 6, 9, 1, 2, 8, 4, 3, 7),
                                Array(6, 1, 4, 7, 5, 8, 3, 9, 2))
                          )

    //
    // Data from
    // http://www.csee.wvu.edu/~ksmani/courses/fa01/random/lecnotes/lecture5.pdf
    //
    val problem3 = Array(
                         // rankWomen
                         Array(
                               Array(1,2,3,4),
                               Array(4,3,2,1),
                               Array(1,2,3,4),
                               Array(3,4,1,2)),
                         
                         // rankMen
                         Array(
                               Array(1,2,3,4),
                               Array(2,1,3,4),
                               Array(1,4,3,2),
                               Array(4,3,1,2))
                         )


    //
    // Data from
    // http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf
    // page 4
    //
    val problem4 = Array(
                         // rankWomen
                         Array(
                               Array(1,5,4,6,2,3),
                               Array(4,1,5,2,6,3),
                               Array(6,4,2,1,5,3),
                               Array(1,5,2,4,3,6),
                               Array(4,2,1,5,6,3),
                               Array(2,6,3,5,1,4)),
                         
                         // rankMen
                         Array(
                               Array(1,4,2,5,6,3),
                               Array(3,4,6,1,5,2),
                               Array(1,6,4,2,3,5),
                               Array(6,5,3,4,2,1),
                               Array(3,1,2,4,5,6),
                               Array(2,3,1,6,5,4)))

    // default problem
    var problem_num = 1
    var problem = van_hentenryck

    // 1..4
    if (args.length > 0) {
      problem_num = args(0).toInt
    }

    problem = problem_num match {
                   case 1 => van_hentenryck
                   case 2 => mathworld
                   case 3 => problem3
                   case 4 => problem4
    }

    val n = problem(0).length

    val rankWomen = problem(0)
    val rankMen = problem(1)

      
    // variables
    val wife    = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val husband = Array.fill(n)(CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      // Comet code are shown in the comments.

      //
      //   forall(m in Men)
      //      cp.post(husband[wife[m]] == m);
      for(m <- 0 until n) {
        cp.add(husband(wife(m)) == m);
      }
      
      //   forall(w in Women)
      //     cp.post(wife[husband[w]] == w);
      for(w <- 0 until n) {
        cp.add(wife(husband(w)) == w);
      }
      
      
      // forall(m in Men, o in Women)
      //    cp.post(rankMen[m,o] < rankMen[m, wife[m]] =>
      //            rankWomen[o,husband[o]] < rankWomen[o,m]);
      for(m <- 0 until n) {
        for(o <- 0 until n) {
          cp.add(
                 (rankMen(m)(wife(m)) >>= rankMen(m)(o))
                 ==> 
                 (rankWomen(o)(husband(o)) <<= rankWomen(o)(m))
                 )
        }
        
      }
      
      // forall(w in Women, o in Men)
      //   cp.post(rankWomen[w,o] < rankWomen[w,husband[w]] =>
      //           rankMen[o,wife[o]] < rankMen[o,w]);
      for(w <- 0 until n) {
        for(o <- 0 until n) {
          cp.add(
                 (rankWomen(w)(husband(w)) >>= rankWomen(w)(o))
                 ==>
                 (rankMen(o)(wife(o)) <<= rankMen(o)(w))
                 )
        }
      }

     } exploration {
       
       cp.binaryFirstFail(wife ++ husband)

       println("wife   :" + wife.mkString(""))
       println("husband:" + husband.mkString(""))
       println()

       numSols += 1
       
     } run()

     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
