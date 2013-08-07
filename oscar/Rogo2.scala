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
import Array._


/**
 *
 * Rogo puzzle solver in OscaR.
 *
 * From http://www.rogopuzzle.co.nz/
 * """
 * The object is to collect the biggest score possible using a given
 * number of steps in a loop around a grid. The best possible score
 * for a puzzle is given with it, so you can easily check that you have
 * solved the puzzle. Rogo puzzles can also include forbidden squares,
 * which must be avoided in your loop.
 * """
 *
 * Also see Mike Trick:
 * "Operations Research, Sudoko, Rogo, and Puzzles"
 * http://mat.tepper.cmu.edu/blog/?p=1302
 *
 *
 * We do the following assumptions:
 *  - the number of steps is "max_steps"
 *  - the optimal sum of points is the value of "best"
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Rogo2 {
 
  val W = 0;
  val B = -1;

  // Global variables
  var problem   : Array[Array[Int]]  = null;
  var rows      : Int = 0;
  var cols      : Int = 0;
  var max_steps : Int = 0;
  var steps     : Int = 0;
  var best      : Int = 0;
  var problem_name : String = "";

  //
  // create all possible connections for this grid 
  //
  def getValidConnections(ROWS: Array[Int], COLS: Array[Int]) : Array[Array[Int]] = 
     (for{i1 <- ROWS
                j1 <- COLS
                i2 <- ROWS
                j2 <- COLS
                if (
                    (abs(j1-j2) == 1 && i1 == i2) 
                    ||
                    (abs(i1-i2) == 1 && j1 % cols == j2 % cols) 
                    )
                  } yield Array(i1*cols+j1, i2*cols+j2)
      ).toArray

  //
  // read problem instance from a file
  //
  def readFile(problem_file: String) {

    val file = scala.io.Source.fromFile(problem_file).getLines
    var linec = 0;
    for {line <- file
         line2 = line.trim()
         tmp = line2.split("[ ,\t]+")
         if tmp.length > 0
         if tmp(0) != "#" 
         if tmp(0) != "%" 
    } {
      linec match {
          case 0 => rows = line2.toInt
          case 1 => cols = line2.toInt; problem = Array.fill(rows)(Array.fill(cols)(0))
          case 2 => max_steps = line2.toInt
          case 3 => best = line2.toInt
          case _ => for(j <- 0 until cols) {
                       problem(linec-4)(j) = tmp(j) match {
                                                 case "B" => B
                                                 case "W" => W
                                                 case  _  => tmp(j).toInt;
                                              }
                    }

      }
      linec += 1;
    }
  }

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    // Default problem
    // Data from
    // Mike Trick: "Operations Research, Sudoko, Rogo, and Puzzles"
    // http://mat.tepper.cmu.edu/blog/?p=1302
    //
    // This has 48 optimal solutions with symmetries;
    // 4 when the path symmetry is removed.
    //
    val default_rows = 5
    val default_cols = 9
    val default_max_steps = 12
    val default_best = 8
    val default_problem = Array(Array(2,W,W,W,W,W,W,W,W),
                                Array(W,3,W,W,1,W,W,2,W),
                                Array(W,W,W,W,W,W,B,W,2),
                                Array(W,W,2,B,W,W,W,W,W),
                                Array(W,W,W,W,2,W,W,1,W))
    val default_problem_name = "Problem Mike Trick"


    if (args.length > 0) {

      val problem_file = args(0)
      readFile(problem_file)
      problem_name = "Problem " + problem_file 

    } else {

      rows = default_rows
      cols = default_cols
      max_steps = default_max_steps
      best = default_best
      problem = default_problem
      problem_name = default_problem_name

    }

    println("\nProblem: " + problem_name)
    println("rows: " + rows + " cols: " + cols + " max_steps: " + max_steps + " best: " + best)
    println()

    val problem_flatten = problem.flatten

    val ROWS   = 0 until rows
    val COLS   = 0 until cols
    val STEPS  = 0 until max_steps
    val STEPS1 = 0 until max_steps - 1

    // valid connections for the problem matrix
    val valid_connections = getValidConnections(ROWS, COLS)

    //
    // variables
    //
    val path       = Array.fill(max_steps)(CPVarInt(cp, 0 to rows*cols-1))
    val points     = Array.fill(max_steps)(CPVarInt(cp, 0 to best))
    val sum_points = CPVarInt(cp, 0 to best)

    //
    // constraints
    //
    var numSols = 0
    cp.maximize(sum_points) subjectTo {

      cp.add(sum_points == sum(points))
      cp.add(sum_points == best)

      for(s <- STEPS) {

        // calculate the points (to maximize)
        cp.add(points(s) == problem_flatten(path(s)))
        
        // ensure that there are no black cells in the path
        cp.add(problem_flatten(path(s)) != B)
      }
      
      cp.add(allDifferent(path), Strong)


      // valid connections
      for(s <- STEPS1) {
        cp.add(table(Array(path(s), path(s+1)), valid_connections))
      }
      // around the corner
      cp.add(table(Array(path(max_steps-1), path(0)), valid_connections))
      
      // Symmetry breaking
      for(s <- 1 until max_steps) {
        cp.add(path(0) < path(s))
      }


    } exploration {
       
      cp.binary(path ++ points ++ Array(sum_points))

      println("sum_points: " + sum_points);
      println("(Adding 1 to coords...)");
      val sol = Array.fill(rows,cols)(0)
      for(s <- STEPS) {
        val p = path(s).value;
        val Array(x, y) = Array(p / cols, p % cols)
        println((x+1) + "," + (y+1) + " (" +  points(s) + " points )");
        sol(x)(y) = 1
      }

      println("\nThe path is marked by 'X's:");
      for(i <- ROWS) {
        for(j <- COLS) {
          var p = if (sol(i)(j) == 1) "X" else  " ";
          val q = problem(i)(j) match {
                    case B => "B"
                    case 0 => "."
                    case _ => problem(i)(j) + ""
                  }
          print("%2s".format(q) + p);
        }
        println();

      }
      println();
      println();

      numSols += 1

    }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
