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
import java.util.Random

/*

  Generates alphametic problems in Oscar.

  This program tries to find alphametic problems 
  given a word list. 

  This use a simple generate (words) and test approach.
  It may take some time to find a correct equation...

  Usage:
     AlphameticGenerate <word_list> <num_words> <base> <start>
     Defaults:  word_list = "/usr/share/dict/words"
                num_words = 3
                base      = 10
                start     = 0


  Assumptions:
  - It generates problems of the form
           NUMBER<1>+NUMBER<2>...+NUMBER<N-1> = NUMBER<N>
    The last number is the sum

  - The words are constructed of characters [A-Za-z]+.

  - The acceptable digits are <start> .. <base>-1
    where <start> is a program parameter, default 0
  
  - Leading digits in each word are constrained to be > 0


  Some of the found alphametic problems:
  * teacup+spitz=cupped
  * looney+relays+shes=ornate
  * dramas+afield=keeled
  * miens+sigma+began=lilacs
  * wrest+risk=tonia
  * redden+ranger+ricing=chinned
  * emend+grooms+salaam+grades=sorrels
  * edged+terser+dirk+rear=lester

  * aced+dachas+price=appear  (base 9)
  * team+eli+lu=gila (base 9, start digit 1)


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object AlphameticGenerate {

  // 
  //  Sum of the words
  // 
  // wordSum(x, "SEND", base, ht) -> 1000*S + 100*E + 10*N + D
  //
  def wordSum(x: Array[CPVarInt], s: String, base: Int, m: Map[Char,Int]) : CPVarInt = {
    val n = s.length
    weightedSum(for(i <- 0 until n) yield pow(base, n-1-i).toInt, 
                for(i <- 0 until n) yield x(m(s(i))))
  }

  /**
    *
    * Read the words from a word list with a specific word length.
    *
    */
  def readWords(word_list: String) : Array[String] = {
    
    println("reading from " + word_list);
    val words = scala.io.Source.fromFile(word_list, "utf-8").getLines

    val rex = "^([A-Za-z]+)$"
    var all_words = List[String]()
    val seen = scala.collection.mutable.HashMap.empty[String, Boolean].withDefaultValue(false)
    for {w <- words
         w2 = w.trim().toLowerCase()
         if w2.length > 0
         if !seen(w2)
         if w2.matches(rex)
    } {    
         all_words ::= w2
         seen += (w2 -> true)
    }
    
    return all_words.reverse.toArray

  }


  //
  // Main solve function
  //
  def solve(problem_in: String = "SEND+MORE=MONEY", base: Int = 10, start: Int = 0) : Int = {

    val cp = CPSolver()

    //
    // data
    //
    val problem_words = problem_in.split("[^A-Za-z]+")
    val p_len = problem_words.length

    println("\nProblem: " + problem_in + " base: " + base + " start digit: " + start)

    // get all unique characters
    val chars = problem_in.filter( _.toString.matches("[a-zA-Z]+")).distinct.sorted
    val n = chars.length
                   
    // create a lookup table: list of (char, index)
    val ht = chars.zipWithIndex.toMap

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, start to base-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(x), Strong)

      // The equation: 
      //    word<0> + word<1> + ... = word<p-1>
      //
      cp.add(
             sum(for(p <- 0 until p_len -1) yield wordSum(x, problem_words(p), base, ht)) ==
             wordSum(x, problem_words(p_len-1), base, ht)
             )

      // ensure that all initial digits > 0
      for(p <- 0 until p_len) {
        cp.add( x(ht(problem_words(p)(0))) > 0)
      }

      
    } exploration {
       
      cp.binaryMaxDegree(x)

      println("\nSolution:")
      println("x:" + x.mkString(""))
      val sep = if (base == 10) "" else " ";

      // solution map
      val sol = chars.zip(x.map(_.value)).toMap
      sol.foreach(println)
      println()

      for (p<- 0 until p_len) {
        val e = problem_words(p)
        println(e + ": " + e.map(sol(_)+"").mkString(sep))
      }
      println()

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions to the problem " + problem_in)
    cp.printStats()

    if (numSols > 0) 1 else 0
    
  }

  def generate(word_list: String, num_words: Int = 3, base: Int = 10, start: Int = 0) {

    println("testProblems")
    val words = readWords(word_list)
    val n = words.length

    var found = 0
    var tested = 0
    val rand = new Random(System.currentTimeMillis());
    while (found == 0) {
      tested += 1
      val a = for(i <- 0 until num_words) yield words(rand.nextInt(n))
      try {
        val problem = (a slice (0, num_words-1)).mkString("+") + "=" + a(num_words-1)
        found += solve(problem, base, start)
      } catch {
        case _ : Throwable => print("")
      }
    }
    
    println("\ntested: " + tested + " found: " + found + "\n")


  }

  def main(args: Array[String]) {
    
    val wordlist  = if (args.length > 0) args(0) else "/usr/share/dict/words";
    val num_words = if (args.length > 1) args(1).toInt else 3;
    val base      = if (args.length > 2) args(2).toInt else 10;
    val start     = if (args.length > 3) args(3).toInt else 0;

    generate(wordlist, num_words, base, start)

  }

}
