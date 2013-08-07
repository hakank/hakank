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

  Divisible by 9 through 1 problem in Oscar.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

  This is a generalized version of oscar.examples.cp.DivisibleBy9Through1
  by Pierre Schaus et. al, which in turn was based on an earlier - and much 
  slower - version of this model. 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object DivisibleBy9Through1 {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var base = 10

    if (args.length > 0) {
      base = args(0).toInt

      if (base > 10) {
        println("\nWarning: base > 10 gives overflow. Resets to base = 10");
        base = 10
      }
    }

    val n  = base - 1
    val m  = pow(base, n).toInt -1
    val m2 = pow(base, n-1).toInt -1
    val coefs = Array.tabulate(n)(i=>pow(base, n-1-i).toInt)

    println("base: " + base)
 
    //
    // variables
    //
    val digits   = Array.fill(n)(CPVarInt(cp, 1 to n))
    val numbers  = Array.fill(n)(CPVarInt(cp, 1 to m))
    val divisors = Array.fill(n)(CPVarInt(cp, 1 to m2))

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {
      
      cp.add(allDifferent(digits), Strong)
      for (i <- 1 to n) {
        cp.add(sum(0 until i)(j => digits(j) * coefs.drop(n-i)(j)) == numbers(i-1))
        cp.add(numbers(i-1) == divisors(i-1) * i)
      }

    } exploration {
       
      cp.binaryFirstFail(digits)

      println("\nSolution:")
      println("digits:" +  digits.mkString(""))
      print("number base 10:" +  numbers.last +  " Base " + base + ": " + 
            digits.map(_.value).mkString(""))
      println()

      numSols += 1

   } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
