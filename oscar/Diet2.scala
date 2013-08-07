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
 * Diet problem in Oscar.
 *
 * Standard OR example.
 *
 * Minimize the cost for the products:
 * Type of                        Calories   Chocolate    Sugar    Fat
 * Food                                      (ounces)     (ounces) (ounces)
 * Chocolate Cake (1 slice)       400           3            2      2
 * Chocolate ice cream (1 scoop)  200           2            2      4
 * Cola (1 bottle)                150           0            4      1
 * Pineapple cheesecake (1 piece) 500           0            4      5
 *
 * This is an alternative version.
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Diet2 {

   def main(args: Array[String]) {

     val cp = CPSolver()

     // data
     val n = 4
     val price  = Array( 50, 20, 30, 80) // in cents
     val limits = Array(500,  6, 10,  8) // requirements for each nutrition type

     // nutritions for each product
     val calories  = Array(400, 200, 150, 500)
     val chocolate = Array(3,2,0,0)
     val sugar     = Array(2,2,4,4)
     val fat       = Array(2,4,1,5)

     val all = Array(calories, chocolate, sugar, fat)

     // variables
     val x = Array.fill(n)(CPVarInt(cp, 0 to 10))
     val cost = weightedSum(price, x)

     // constraints
     cp.minimize(cost) subjectTo {

       // handle the nutrition requirements
       for(i <- 0 until n) (
           cp.add(weightedSum(all(i), x) >= limits(i))
       )

    } exploration {
       
       cp.binaryFirstFail(x)
       println(x.mkString(" "))
       
    }

    println()
    cp.printStats()

  }

}
