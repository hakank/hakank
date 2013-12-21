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

  Eq 10 problem in Oscar.

  Standard benchmark problem.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Eq10 {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 7

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to 9))
    val Array(x1,x2,x3,x4,x5,x6,x7) = x

    //
    // constraints
    //
    cp.solve subjectTo {

      cp.add(x1*98527+x2*34588+x3*5872+x5*59422+x7*65159
                 == x4*30704+x6*29649+1547604)
      
      cp.add(x2*98957+x3*83634+x4*69966+x5*62038+x6*37164+x7*85413
                 == x1*93989+1823553)
      
      cp.add(x1*10949+x2*77761+x5*67052+900032
                 == x3*80197+x4*61944+x6*92964+x7*44550)
      
      cp.add(x1*73947+x3*84391+x5*81310
                 == x2*96253+x4*44247+x6*70582+x7*33054+1164380)
      
      cp.add(x3*13057+x4*42253+x5*77527+x7*96552
                 == x1*60152+x2*21103+x6*97932+1185471)

      cp.add(x1*66920+x4*55679+1394152
                 == x2*64234+x3*65337+x5*45581+x6*67707+x7*98038)
      
      cp.add(x1*68550+x2*27886+x3*31716+x4*73597+x7*38835
                 == x5*88963+x6*76391+279091)
      
      cp.add(x2*76132+x3*71860+x4*22770+x5*68211+x6*78587
                 == x1*48224+x7*82817+480923)
      
      cp.add(x2*94198+x3*87234+x4*37498+519878
                 == x1*71583+x5*25728+x6*25495+x7*70023)
      
      cp.add(x1*78693+x5*38592+x6*38478+361921
                 == x2*94129+x3*43188+x4*82528+x7*69025)
      
   
    } search {
       
      binaryStatic(x)

    } onSolution {
      
      println("x:" + x.mkString(""))
    
    }

    println(cp.start())

  }

}
