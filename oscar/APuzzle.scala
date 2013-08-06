/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._


/**
 
   Number puzzle in Oscar

   From "God plays dice"
   "A puzzle"
   http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
   And the sequel "Answer to a puzzle"
   http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/
   
   This problem instance was taken from the latter blog post.
   (Problem 1)
   
   """
   8809 = 6
   7111 = 0
   2172 = 0
   6666 = 4
   1111 = 0
   3213 = 0
   7662 = 2
   9312 = 1
   0000 = 4
   2222 = 0
   3333 = 0
   5555 = 0
   8193 = 3
   8096 = 5
   7777 = 0
   9999 = 4
   7756 = 1
   6855 = 3
   9881 = 5
   5531 = 0
   
   2581 = ?
   """
   
   Note:
   This model yields 10 solutions, since x4 is not
   restricted in the constraints.
   All solutions has x assigned to the correct result.
   
   
   (Problem 2)
   The problem stated in "A puzzle"
   http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
   is
   """
   8809 = 6
   7662 = 2
   9312 = 1
   8193 = 3
   8096 = 5
   7756 = 1
   6855 = 3
   9881 = 5
   
   2581 = ?
   """
   This problem instance yields two different solutions of x,
   one is the same (correct) as for the above problem instance,
   and one is not.
   This is because here x0,x1,x4 and x9 are underdefined.


   @author Hakan Kjellerstrand hakank@gmail.com
   http://www.hakank.org/oscar/
 
 */
object APuzzle {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 10

    // Problem type
    var p = 1 // 1,2,3,4
    if (args.length > 0) {
      p = args(0).toInt
      if (p < 1 || p > 4) {
        println("Valid p's are 1..4. Set to p=1")
        p = 1
      }
    }
    
    val all = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val Array(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) = all

    // the unknown, i.e. 2581 = x
    val x = CPVarInt(cp, 0 to n-1)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      if (p==1) {
        
        // Problem 1 (basic approach)
        println("Problem 1 (basic approach)")
        cp.add(x8+x8+x0+x9 == 6)
        cp.add(x7+x1+x1+x1 == 0)
        cp.add(x2+x1+x7+x2 == 0)
        cp.add(x6+x6+x6+x6 == 4)
        cp.add(x1+x1+x1+x1 == 0)
        cp.add(x3+x2+x1+x3 == 0)
        cp.add(x7+x6+x6+x2 == 2)
        cp.add(x9+x3+x1+x2 == 1)
        cp.add(x0+x0+x0+x0 == 4)
        cp.add(x2+x2+x2+x2 == 0)
        cp.add(x3+x3+x3+x3 == 0)
        cp.add(x5+x5+x5+x5 == 0)
        cp.add(x8+x1+x9+x3 == 3)
        cp.add(x8+x0+x9+x6 == 5)
        cp.add(x7+x7+x7+x7 == 0)
        cp.add(x9+x9+x9+x9 == 4)
        cp.add(x7+x7+x5+x6 == 1)
        cp.add(x6+x8+x5+x5 == 3)
        cp.add(x9+x8+x8+x1 == 5)
        cp.add(x5+x5+x3+x1 == 0)
        
        // The unknown
        cp.add(x2+x5+x8+x1 == x)

      } else if (p == 2) {
        // Problem 1 (alternative approach)
        println("Problem 1 (alternative approach)")
        val problem1 = Array(
                             Array(8,8,0,9, 6),
                             Array(7,1,1,1, 0),
                             Array(2,1,7,2, 0),
                             Array(6,6,6,6, 4),
                             Array(1,1,1,1, 0),
                             Array(3,2,1,3, 0),
                             Array(7,6,6,2, 2),
                             Array(9,3,1,2, 1),
                             Array(0,0,0,0, 4),
                             Array(2,2,2,2, 0),
                             Array(3,3,3,3, 0),
                             Array(5,5,5,5, 0),
                             Array(8,1,9,3, 3),
                             Array(8,0,9,6, 5),
                             Array(7,7,7,7, 0),
                             Array(9,9,9,9, 4),
                             Array(7,7,5,6, 1),
                             Array(6,8,5,5, 3),
                             Array(9,8,8,1, 5),
                             Array(5,5,3,1, 0))

       for(i <- 0 until problem1.length) {
         cp.add( sum(for(j <- 0 until 4) yield all(problem1(i)(j))
                     ) == problem1(i)(4) )
      }

      cp.add(all(2)+all(5)+all(8)+all(1) == x);

      } else if (p == 3) {

        // Problem 2 (basic approach)
        println("Problem 2 (basic approach)")

        cp.add(x8+x8+x0+x9 == 6)
        cp.add(x7+x6+x6+x2 == 2)
        cp.add(x9+x3+x1+x2 == 1)
        cp.add(x8+x1+x9+x3 == 3)
        cp.add(x8+x0+x9+x6 == 5)
        cp.add(x7+x7+x5+x6 == 1)
        cp.add(x6+x8+x5+x5 == 3)
        cp.add(x9+x8+x8+x1 == 5)
        
        // The unknown
        cp.add(x2+x5+x8+x1 == x)


      } else {
  
        // Problem 2 (alternative approach)
        println("Problem 2 (alternative approach)")
        val problem2 = Array(
                             Array(8,8,0,9, 6),
                             Array(7,6,6,2, 2),
                             Array(9,3,1,2, 1),
                             Array(8,1,9,3, 3),
                             Array(8,0,9,6, 5),
                             Array(7,7,5,6, 1),
                             Array(6,8,5,5, 3),
                             Array(9,8,8,1, 5))
        
        for(i <- 0 until problem2.length) {
          cp.add( sum(for(j <- 0 until 4) yield all(problem2(i)(j))
                      ) == problem2(i)(4) 
                  )
        }
                
        cp.add(all(2)+all(5)+all(8)+all(1) == x)
        
      }


    } exploration {
       
      cp.binary(all)

      println("all:" + all.mkString("") + "  x:" + x)

      numSols += 1
       
     } run()
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
