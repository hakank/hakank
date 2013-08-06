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
 *
 * Note: A slightly different approach is in Diet2.scala
 *
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Diet {

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

     // variables
     val x = Array.fill(n)(CPVarInt(cp, 0 to 10))
     val cost = weightedSum(price, x)

     // constraints
     cp.minimize(cost) subjectTo {

       cp.add(weightedSum(calories, x) >= limits(0))
       cp.add(weightedSum(chocolate, x) >= limits(1))
       cp.add(weightedSum(sugar, x) >= limits(2))
       cp.add(weightedSum(fat, x) >= limits(3))

     } exploration {
       
       cp.binaryFirstFail(x)
       println(x.mkString(" "))
       
     }

     println()
     cp.printStats()
   }

}
