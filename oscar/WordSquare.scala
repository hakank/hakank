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
import scala.util.matching._

/*

  WordSquare in Oscar.

  From http://en.wikipedia.org/wiki/Word_square
  """
  A word square is a special case of acrostic. It consists of a set of words,
  all having the same number of letters as the total number of words (the
  'order' of the square); when the words are written out in a square grid
  horizontally, the same set of words can be read vertically.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object WordSquare {


  /**
    *
    * Read the words from a word list with a specific word length.
    *
    */
  def readWords(word_list: String, word_len: Int, regex: String) : Array[String] = {
    
    println("reading from " + word_list + " (size: " + word_len + ")");
    scala.io.Source.fromFile(word_list, "utf-8").getLines.
                    filter(_.length == word_len).
                    map(_.toLowerCase).
                    filter(w => w.matches(regex)).
                    toList.distinct.toArray

  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val word_list   = if (args.length > 0) args(0) else "/usr/share/dict/words"
    val word_len    = if (args.length > 1) args(1).toInt else 5
    val num_to_show = if (args.length > 2) args(2).toInt else 20

    println("word_len:" + word_len)

    val WORDLEN = 0 until word_len

    // Convert letters => digits
    val d = ('a' to 'z').zipWithIndex.toMap 


    // Read the word list
    val words = readWords(word_list, word_len,"^([a-zA-Z]+)$")
    val num_words = words.length
    println("number of words: " + num_words)

    //
    // variables
    //

    // word matrix 
    val A = Array.tabulate(num_words,word_len)((i,j) => d(words(i)(j)))
    // the selected words
    val E = Array.fill(word_len)(CPVarInt(cp, 0 to num_words))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(E),Weak)

      // now find the connections
      for{i <- WORDLEN
          j <- WORDLEN} {
        cp.add(A(E(i))(CPVarInt(cp,j)) == A(E(j))(CPVarInt(cp,i)))
      }


    } exploration {

      cp.binaryFirstFail(E)
        
      println("solution #" + (numSols+1))
      println(E.map(e=>words(e.value)).mkString("\n"))
      println()

      numSols += 1
      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop();
      }

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

 }

}
