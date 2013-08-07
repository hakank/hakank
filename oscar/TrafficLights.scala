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

  Traffic lights problem (CSPLib #16) in Oscar.

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of the traffic
  lights are for the vehicles and can be represented by the variables V1 to V4 with domains
  {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are
  for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
  
  The constraints on these variables can be modelled by quaternary constraints on
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
  
  It would be interesting to consider other types of junction (e.g. five roads
  intersecting) as well as modelling the evolution over time of the traffic light sequence.
  ...
  
  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.
  
  (V1,P1,V2,P2,V3,P3,V4,P4) =
  {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
  [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}
  The problem has relative few constraints, but each is very
  tight. Local propagation appears to be rather ineffective on this
  problem.  
  """
 
  Here are the four solutions from this model
  Solution:
  V:  0 2 0 2
  P:  0 2 0 2
  r r g g r r g g 

  Solution:
  V:  1 3 1 3
  P:  0 0 0 0
  ry r y r ry r y r 

  Solution:
  V:  2 0 2 0
  P:  2 0 2 0
  g g r r g g r r 

  Solution:
  V:  3 1 3 1
  P:  0 0 0 0
  y r ry r y r ry r 


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object TrafficLights {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 4

    val r = 0;
    val ry = 1;
    val g = 2;
    val y = 3;

    val lights = Array("r", "ry", "g", "y")

    // The allowed combinations
    val allowed = Array(Array(r,r,g,g),
                        Array(ry,r,y,r),
                        Array(g,g,r,r),
                        Array(y,r,ry,r))


    //
    // variables
    // 
    val V = Array.fill(n)(CPVarInt(cp, 0 to n-1))
    val P = Array.fill(n)(CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      for(i <- 0 until n) {
        val j = (1+i) % n
        cp.add( table(Array(V(i),P(i),V(j),P(j)), allowed), Strong)
      }

    } exploration {
       
      cp.binary( V ++ P)

      println("\nSolution:")
      println("V: " + V.mkString(""))
      println("P: " + P.mkString(""))
      for(i <- 0 until n) {
        print(lights(V(i).value) + " " + lights(P(i).value) + " ")
      }
      println()

      numSols += 1

    } run()

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
