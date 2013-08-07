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
 *
 * ABC... Endview problem in Oscar.
 * 
 * This is also knows as "Easy as ABC" and "Last Man Standing".
 * 
 * From Fun With Puzzles: "ABC End View: A1"
 * http://www.funwithpuzzles.com/2009/12/abcd-end-view-a1.html
 * """
 * This the classical puzzle and appeared in many World Puzzle Championships.
 * 
 * This puzzle is also known with following names
 * 1. Easy as ABC
 * 
 * "ABC End View" Puzzle Instructions:
 * Enter the letters ABC such that each letter is exactly once, in all of the 
 * rows and columns. One cells will remain empty in each row and column. 
 * The letters outside the grid show which letter you come across first 
 * from that direction.
 * 
 *       ABC End View
 * 
 *            A               
 *        _ _ _ _
 *      C _ _ _ _
 *      C _ _ _ _ B
 *        _ _ _ _
 *          B
 * """
 *
 * Note: There are some problem instances below that use A..D and
 *       5x5 or 6x6 grid which means that there may be 2 empty cells
 *       before/after.
 * 
 * Also see:
 *   http://www.janko.at/Raetsel/AbcEndView/index.htm
 *
 * The diagonal constraint means that the two diagonal should
 * also be allDifferent (except empty cells). See problem11
 * for an example: http://www.janko.at/Raetsel/AbcEndView/168.a.htm
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ABCEndview {

 
    class Problem(in_n: Int,
                  in_max_letter: Int,
                  in_row_upper: Array[Int],
                  in_row_lower: Array[Int],
                  in_col_left: Array[Int],
                  in_col_right: Array[Int],
                  in_diagonal: Boolean,
                  in_name: String) {
        val n = in_n
        val max_letter = in_max_letter
        val row_upper  = in_row_upper
        val row_lower  = in_row_lower
        val col_left   = in_col_left
        val col_right  = in_col_right
        val diagonal   = in_diagonal
        val name       = in_name

        val letters = "_ABCDEF".split("")

        def presLine(a: Array[Int]) = 
          (for(i <- 0 until n) 
            yield letters(a(i)+1)
           ) mkString(" ")


        override def toString =
          "problem   : " + name       + "\n" +
          "n         : " + n          + "\n" + 
          "max_letter: " + max_letter + "\n" +
          "row_upper : " + presLine(row_upper) + "\n" +
          "row_lower : " + presLine(row_lower) + "\n" +
          "col_left  : " + presLine(col_left)  + "\n" +
          "col_right : " + presLine(col_right) + "\n" +
          "diagonal  : " + diagonal            + "\n"
          
    }


    //
    // a start with letter c and accept d empty slots.
    // 
    def startWith(cp: CPSolver, a: Array[CPVarInt], c: Int, d: Int) = {
      val n = a.length
      if (c > 0) {
        val i = CPVarInt(cp, 0 until d)
        cp.add(a(i) == c)
        for(j <- 0 until n) {
          cp.add((i >>= j) ==> (a(j) === 0))
        }
      }
    }


    //
    // a ends with letter c and accept d empty slots
    //
    def endWith(cp: CPSolver, a: Array[CPVarInt], c: Int, d: Int) = {
      val n = a.length
      if (c > 0) {
        val i = CPVarInt(cp, n-d until n)
        cp.add(a(i) == c)
        for(j <- 0 until n) {
          cp.add((i <<= j) ==> (a(j) === 0))
        }
      }
    }
  
    //
    // main
    // 
    def main(args: Array[String]) {

      val a = 1; val b = 2; val c = 3; val d = 4; val e = 5; val f = 6;

      //
      // problem instances
      //

      // Problem instance from
      // http://www.funwithpuzzles.com/2009/12/abcd-end-view-a1.html
      // This is a 4x4 with 1 blank
      val problem1 = new Problem(4, // n
                                 c, // max_letter
                                 Array(0,0,a,0), // row_upper
                                 Array(0,b,0,0), // row_lower
                                 Array(0,c,c,0), // col_left
                                 Array(0,0,b,0), // col_right
                                 false,          // has diagonal constraint?
                                 "problem1")


      // From Dan Moore: Brainpower Bible
      // (introduction example)
      // Note: This is a 5x5 grid with 2 blanks
      val problem2 = new Problem(5,
                                 c,
                                 Array(c,c,a,c,0),
                                 Array(b,b,0,a,0),
                                 Array(0,a,0,0,0),
                                 Array(0,b,a,b,0),
                                 false,
                                 "problem2")


      
      // From Dan Moore: Brainpower Bible
      // Alfa 1 (5x5)
      val problem3 = new Problem(5,
                                 c,
                                 Array(0,b,b,c,0),
                                 Array(b,c,a,a,b),
                                 Array(0,0,b,b,c),
                                 Array(0,b,0,a,b),
                                 false,
                                 "problem3")


      // From Dan Moore: Brainpower Bible
      // Alfa 2 (5x5)
      val problem4 = new Problem(5,
                                 c,
                                 Array(b,0,0,c,b),
                                 Array(0,a,b,a,c),
                                 Array(0,0,0,a,b),
                                 Array(a,b,0,0,0),
                                 false,
                                 "problem4")
                                

      
      // From Dan Moore: Brainpower Bible
      // Delta 1 (6x6)
      val problem5 = new Problem(6,
                                 d,
                                 Array(d,d,a,0,c,a),
                                 Array(0,0,0,c,0,0),
                                 Array(d,0,d,0,0,a),
                                 Array(0,0,a,d,c,c),
                                 false,
                                 "problem5")

      
      // from Dan Moore: Brainpower Bible
      // Delta 2 (6x6)
      val problem6 = new Problem(6,
                                 d,
                                 Array(0,b,d,d,0,c),
                                 Array(b,0,0,b,c,d),
                                 Array(a,d,c,0,b,d),
                                 Array(0,0,b,b,0,0),
                                 false,
                                 "problem6")
        
      
      // From http://www.cross-plus-a.com/puzzles.htm#EasyAsABC
      // (6x6)
      val problem7 = new Problem(6,
                                 e,
                                 Array(a,0,0,e,0,0),
                                 Array(0,b,c,0,0,a),
                                 Array(0,0,a,d,0,0),
                                 Array(e,0,b,e,0,0),
                                 false,
                                 "problem7")
                                   

      // From http://www.janko.at/Raetsel/AbcEndView/046.a.htm
      // ABC End View Nr. 46
      // (7x7)
      // (Difficulty 8, "schwer")
      val problem8 = new Problem(7,
                                 e,
                                 Array(0,0,d,c,0,0,c),
                                 Array(0,0,e,d,d,c,0),
                                 Array(0,e,c,c,e,b,0),
                                 Array(0,0,b,0,0,c,0),
                                 false,
                                 "problem8")
      

      // http://puzzlepicnic.com/puzzle?2712
      // 
      val problem9 = new Problem(5,
                                 d,
                                 Array(0,0,a,a,0),
                                 Array(0,c,c,0,0),
                                 Array(0,d,d,0,0),
                                 Array(0,0,b,b,0),
                                 false,
                                 "problem9")

      // http://www.janko.at/Raetsel/AbcEndView/209.a.htm
      // ABC End View Nr. 209
      // (7x7)
      // (Difficulty 8, "schwer")

      val problem10 = new Problem(7,
                                  e,
                                  Array(a,0,b,0,0,c,b),
                                  Array(0,a,0,b,b,d,0),
                                  Array(a,c,a,0,e,0,e),
                                  Array(0,d,0,c,0,e,0),
                                 false,
                                  "problem10")
        

      // http://www.janko.at/Raetsel/AbcEndView/168.a.htm
      // ABC End View Nr. 168
      // (7x7)
      // (Difficulty 8, "schwer")
      //
      // Note: this has also the diagonal constraints.
      // 
      //
      val problem11 = new Problem(7,
                                  f,
                                  Array(a,0,c,e,f,a,0),
                                  Array(0,d,0,c,0,0,e),
                                  Array(a,0,0,c,e,d,0),
                                  Array(0,c,b,0,0,e,0),
                                  true,
                                  "problem11")
        


      // all problems
      val problems = Array(problem1,problem2,problem3,problem4,
                           problem5,problem6,problem7,problem8,
                           problem9,problem10,problem11)



      if (args.length > 0) {
        var choice = args(0).toInt
        val n = problems.length
        if (choice < 1 || choice > n) {
          println("\nSorry, problem are 1.." + n + ". Setting problem to #1.")
          choice = 1
        }
        solve(problems(choice-1))
      } else {
        for(problem <- problems) {
          solve(problem)
        }
      }

    }

    def solve(problem: Problem) {

      val cp = CPSolver()

      //
      // data
      //     
      val a = 1; val b = 2; val c = 3; val d = 4; val e = 5; val f = 6;
      val str = "_ABCDEF".split("")
      println("str: " + str.mkString(""))


      val n          = problem.n
      val max_letter = problem.max_letter
      val row_upper  = problem.row_upper
      val row_lower  = problem.row_lower
      val col_left   = problem.col_left
      val col_right  = problem.col_right
      val diagonal   = problem.diagonal
      val problem_name = problem.name

 
      println("\nSolving " + problem)


      // derived 
      val RANGE = 0 until n

      // number of accepted empty cells before/after
      val dist = n - max_letter + 1

      // for global cardinality (Latin square)
      val counts = Array(n-max_letter) ++ Array.fill(max_letter)(1)

      // println("dist: " + dist)
      // println("counts: " + counts.mkString(" "))


      //
      // variables
      //

      // 0 -> empty cell, 1..max_letter: the letters
      val x = Array.fill(n,n)(CPVarInt(cp, 0 to max_letter))
      val x_flat = x.flatten

      //
      // constraints
      //
      var numSols = 0;

      cp.solve subjectTo {

        //
        // Latin square (except 0)
        //
        for(i <- RANGE) {
          cp.add(gcc( for(j <- RANGE) yield x(i)(j), RANGE, counts, counts), Strong)
          cp.add(gcc( for(j <- RANGE) yield x(j)(i), RANGE, counts, counts), Strong)
        }

        if (diagonal) {
          cp.add(gcc( for(i <- RANGE) yield x(i)(i), RANGE, counts, counts), Strong)
          cp.add(gcc( for(i <- RANGE) yield x(i)(n-i-1), RANGE, counts, counts), Strong)
        }

        //
        // The hints
        //
        for(j <- RANGE) {
          val tmp = for(i <- RANGE) yield x(i)(j);
          startWith(cp, tmp, row_upper(j), dist)
          endWith(cp, tmp, row_lower(j), dist)
        }

        for(i <- RANGE) {
          val tmp = for(j <- RANGE) yield x(i)(j);
          startWith(cp, tmp, col_left(i), dist)
          endWith(cp, tmp, col_right(i), dist)
        }


      } exploration {

        /*
        println("\nBefore labeling: ")
        for(i <- RANGE) {
           println(x(i).mkString(""))
        }
        println()
        */
  
        while (!allBounds(x_flat)) {

          // all unbound variables
          val notbound = x_flat.filterNot(_.isBound)

          // "max regret"
          val y = argMax(notbound)(v=>v.max-v.min).last

          //    
          // value selection
          // 
          val vMax = y.max
          val v = vMax
    	  cp.branch {
            cp.post(y == v)
          } {
            cp.post(y != v)
          }

        }


        println("Solution:")
        for(i <- RANGE) {
          println(x(i).mkString(""))
        }
        println()

        for(i <- RANGE) {
          for(j <- RANGE) {
            print(str(1+x(i)(j).value) + " ")
          }
          println()
        }
        println()
          
        numSols +=1

     } run()
 
     
     println("\nIt was " + numSols + " solution(s).")
     cp.printStats()

  }

}
