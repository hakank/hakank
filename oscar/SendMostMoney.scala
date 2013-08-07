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
 * SEND+MOST=MONEY problem in Oscar.
 *
 * The objective is to maximize MONEY and
 * print all solutions for that value.
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMostMoney {

   def main(args: Array[String]) {
     val money = send_most_money(0)
     println("\nGot maximum value of MONEY: " + money)
     println("Now we check for all solutions...")
     send_most_money(money)
   }

   def send_most_money(money: Int) : Int = {

      val cp = CPSolver()

      // variables
      val S = CPVarInt(cp, 0 to 9)
      val E = CPVarInt(cp, 0 to 9)
      val N = CPVarInt(cp, 0 to 9)
      val D = CPVarInt(cp, 0 to 9)
      val M = CPVarInt(cp, 0 to 9)
      val O = CPVarInt(cp, 0 to 9)
      val T = CPVarInt(cp, 0 to 9)
      val Y = CPVarInt(cp, 0 to 9)

      val all = Array(S,E,N,D,M,O,T,Y)

      val Money = M*10000 + O*1000 + N*100 + E*10 + Y
      var this_money = money
      if (money > 0) {
        cp.solve subjectTo {

          // constraints
          cp.add(       S*1000 + E*100 + N*10 + D +
                        M*1000 + O*100 + S*10 + T ==
              M*10000 + O*1000 + N*100 + E*10 + Y)
          cp.add(S > 0)
          cp.add(M > 0)
          cp.add(Money == money)
    	  cp.add(allDifferent(all), Strong)

         } exploration {

           cp.binaryFirstFail(all)

           println(all.mkString(""))
           println("Money: " + Money)

        } run()


      } else {

        cp.maximize(Money) subjectTo {

          // constraints
          cp.add(       S*1000 + E*100 + N*10 + D +
                        M*1000 + O*100 + S*10 + T ==
              M*10000 + O*1000 + N*100 + E*10 + Y)
          cp.add(S > 0)
          cp.add(M > 0)
    	  cp.add(allDifferent(all), Strong)

         } exploration {

           cp.binaryFirstFail(all)

           println(all.mkString(""))
           println("Money: " + Money)
           this_money = Money.value
        }

      }
      
      println()
      cp.printStats()

      return this_money

   }

}
