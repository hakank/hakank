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
import scala.math._
import Array._


/**
 *
 * Pandigital numbers in OscaR.
 *
 * From Albert H. Beiler 'Recreations in the Theory of Numbers',
 * quoted from http://www.worldofnumbers.com/ninedig1.htm
 * """
 * Chapter VIII : Digits - and the magic of 9
 *
 * The following curious table shows how to arrange the 9 digits so that
 * the product of 2 groups is equal to a number represented by the
 * remaining digits.
 *
 *   12 x 483 = 5796
 *   42 x 138 = 5796
 *   18 x 297 = 5346
 *   27 x 198 = 5346
 *   39 x 186 = 7254
 *   48 x 159 = 7632
 *   28 x 157 = 4396
 *   4 x 1738 = 6952
 *   4 x 1963 = 7852
 * """
 *
 * Also see MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
 * """
 * A number is said to be pandigital if it contains each of the digits
 * from 0 to 9 (and whose leading digit must be nonzero). However,
 * "zeroless" pandigital quantities contain the digits 1 through 9.
 * Sometimes exclusivity is also required so that each digit is
 * restricted to appear exactly once.
 * """
 *
 * Wikipedia: http://en.wikipedia.org/wiki/Pandigital_number
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object PandigitalNumbers {

  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPVarInt], base: Int=10) = sum(
      Array.tabulate(t.length)(i=> t(i)*pow(base, t.length-i-1).toInt))

  //
  // solve each sub equation
  //
  def solve(base: Int = 10, start: Int = 1, len1: Int = 1, len2: Int = 2) {

    val cp = CPSolver()

    //
    // data
    //
    val max_d   = base-1
    val x_len   = max_d + 1 - start
    val max_num = (pow(base,4)-1).toInt

    //
    // variables
    //
    val num1 = CPVarInt(cp, 1 to max_num)
    val num2 = CPVarInt(cp, 1 to max_num)
    val res  = CPVarInt(cp, 1 to max_num)
    // the digits
    val x    = Array.fill(x_len)(CPVarInt(cp, start to max_d))

    // for labeling    
    val all = x ++ Array(num1, num2, res)

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(x))

      cp.add(num1 == toNum((for{i <- 0 until len1} yield x(i)), base))
      cp.add(num2 == toNum((for{i <- len1 until len1+len2} yield x(i)), base))
      cp.add(res  == toNum((for{i <- (len1+len2) until x_len} yield x(i)), base))
      cp.add(num1*num2 == res)

      // no number must start with 0
      cp.add(x(0) > 0)
      cp.add(x(len1) > 0)
      cp.add(x(len1+len2) > 0)
      
      // symmetry breaking
      cp.add(num1 < num2)


    } exploration {
       
      cp.binaryFirstFail(all)

      println(num1 + " +" + num2 + " =" + res )

      numSols += 1

    } run()

    // println("\nIt was " + numSols + " solutions.")
    // cp.printStats()

  }

  def main(args: Array[String]) {

    var base = 10
    var start = 1

    if(args.length > 0) {
      base = args(0).toInt
    }

    if(args.length > 1) {
      start = args(1).toInt
    }

    println("base: " + base + " start: " + start)

    var x_len = base - 1 + 1-start;
    for(len1 <- 0 to x_len) {
      for(len2 <- 0 to x_len) {
        if (x_len > len1 + len2
            && len1 > 0 && len2 > 0) {
          // We need to catch those cases where there is no solution
          try {
            solve(base, start, len1, len2)
          } catch {
            case e: Exception => ""
          } 
        }
      }
    }

  } // end main

}
