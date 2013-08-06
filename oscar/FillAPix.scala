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

import scala.io.Source._

/**
 *
 * Fill-a-pix problem in Oscar.
 *
 *
 * From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
 * """
 * Each puzzle consists of a grid containing clues in various places. The
 * object is to reveal a hidden picture by painting the squares around each
 * clue so that the number of painted squares, including the square with
 * the clue, matches the value of the clue.
 * """
 *
 * http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
 * """
 * Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated
 * picture hidden inside. Using logic alone, the solver determines which
 * squares are painted and which should remain empty until the hidden picture
 * is completely exposed.
 * """
 *
 * Fill-a-pix History:
 * http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object FillAPix {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    //
    //
    val X = -1

    // Default problem.
    // Puzzle 1 from
    // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
    val default_n = 10
    val default_puzzle = List(List(X,X,X,X,X,X,X,X,0,X),
                              List(X,8,8,X,2,X,0,X,X,X),
                              List(5,X,8,X,X,X,X,X,X,X),
                              List(X,X,X,X,X,2,X,X,X,2),
                              List(1,X,X,X,4,5,6,X,X,X),
                              List(X,0,X,X,X,7,9,X,X,6),
                              List(X,X,X,6,X,X,9,X,X,6),
                              List(X,X,6,6,8,7,8,7,X,5),
                              List(X,4,X,6,6,6,X,6,X,4),
                              List(X,X,X,X,X,X,3,X,X,X))
    
    // for the actual problem
    var n      = default_n
    var puzzle = default_puzzle


    // read problem from file
    if (args.length > 0) {
      println("Read from file: " + args(0))
    
      val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
      n = lines(0).toInt
      println("n:" + n)
      puzzle = lines.tail.map(_.split("").tail.toList.map(i=>if (i == ".") X else i.toInt))
    }

    //
    // decision variables
    // 
    val pict = Array.fill(n,n)(CPVarInt(cp, 0 to 1))
    val pict_flat = pict.flatten

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      val tmp = List(-1,0,1)

      for(i <- 0 until n;
          j <- 0 until n if puzzle(i)(j) >= 0) {            
            cp.add(sum( for{ a <- tmp; 
                             b <- tmp 
                             if ( (i+a >= 0) && (j+b >=  0) &&
                                  (i+a < n)  && (j+b < n)) 
                               } yield pict(i+a)(j+b) ) == puzzle(i)(j))
      }


    } exploration {
       
      cp.binaryFirstFail(pict.flatten)

      println("\nSolution:")
      println(pict.map(i=>i.map(j => if (j.value == 1) "#" else " ").mkString("")).mkString("\n"))

      numSols += 1
       
     } run()

     println("\nIt was " + numSols + " solutions.\n")
     cp.printStats()
   }

}
