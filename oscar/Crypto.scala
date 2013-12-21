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

  Crypto problem in Oscar.
    
  This is the standard benchmark "crypto" problem.
  
  Problem formulation from GLPK:s model cryto.mod:
  """
  This problem comes from the newsgroup rec.puzzle.
  The numbers from 1 to 26 are assigned to the letters of the alphabet.
  The numbers beside each word are the total of the values assigned to
  the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
  5, 9, 20 and 13, or any other combination that add up to 47).
  Find the value of each letter under the equations:
  
  BALLET  45     GLEE  66     POLKA      59     SONG     61
  CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
  CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
  FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
  FUGUE   50     OPERA 65     SOLO       37     WALTZ    34
  
  Solution:
  A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
  5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18
  
  Reference:
  Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
  Simple problems, the crypto-arithmetic puzzle ALPHACIPHER.
  """

  For another approach see Crypto2.scala.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Crypto {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num_letters = 26

    val BALLET     =  45
    val CELLO      =  43
    val CONCERT    =  74
    val FLUTE      =  30
    val FUGUE      =  50
    val GLEE       =  66
    val JAZZ       =  58
    val LYRE       =  47
    val OBOE       =  53
    val OPERA      =  65
    val POLKA      =  59
    val QUARTET    =  50
    val SAXOPHONE  = 134
    val SCALE      =  51
    val SOLO       =  37
    val SONG       =  61
    val SOPRANO    =  82
    val THEME      =  72
    val VIOLIN     = 100
    val WALTZ      =  34



    //
    // variables
    //
    val LD = Array.fill(num_letters)(CPVarInt(cp, 1 to num_letters))

    // Note: D is not used in the constraints below
    // Note: Scala only allows for max 22-tuple... 
    // val Array(a,b,c,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) = LD
    val A =  LD(0); val B =  LD(1); val C =  LD(2); // val D =  LD(3);
    val E =  LD(4); val F =  LD(5); val G =  LD(6); val H =  LD(7); 
    val I =  LD(8); val J =  LD(9); val K = LD(10); val L = LD(11); 
    val M = LD(12); val N = LD(13); val O = LD(14); val P = LD(15); 
    val Q = LD(16); val R = LD(17); val S = LD(18); val T = LD(19); 
    val U = LD(20); val V = LD(21); val W = LD(22); val X = LD(23); 
    val Y = LD(24); val Z = LD(25);


    //
    // constraints
    //
    var numSols = 0

    cp.solve subjectTo {

      cp.add(allDifferent(LD), Strong)

      cp.add(            B + A + L + L + E + T == BALLET)
      cp.add(                C + E + L + L + O == CELLO)
      cp.add(        C + O + N + C + E + R + T == CONCERT)
      cp.add(                F + L + U + T + E == FLUTE)
      cp.add(                F + U + G + U + E == FUGUE)
      cp.add(                    G + L + E + E == GLEE)
      cp.add(                    J + A + Z + Z == JAZZ)
      cp.add(                    L + Y + R + E == LYRE)
      cp.add(                    O + B + O + E == OBOE)
      cp.add(                O + P + E + R + A == OPERA)
      cp.add(                P + O + L + K + A == POLKA)
      cp.add(        Q + U + A + R + T + E + T == QUARTET)
      cp.add(S + A + X + O + P + H + O + N + E == SAXOPHONE)
      cp.add(                S + C + A + L + E == SCALE)
      cp.add(                    S + O + L + O == SOLO)
      cp.add(                    S + O + N + G == SONG)
      cp.add(        S + O + P + R + A + N + O == SOPRANO)
      cp.add(                T + H + E + M + E == THEME)
      cp.add(            V + I + O + L + I + N == VIOLIN)
      cp.add(                W + A + L + T + Z == WALTZ)


    } search {
       
      binaryMaxDegree(LD)
      
    } onSolution {
      
      println("\nSolution:")
      println("LD:" + LD.mkString(""))

      numSols += 1

   } 
    
   println(cp.start())

  }

}
