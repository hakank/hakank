package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*
  Arch friends puzzle (Dell Logic Puzzles) in Oscar.

  Problem formulation from 
  http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four shoe 
  purchases to her friend Aurora. Aurora just loves the four different kinds 
  of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
  and suede sandals), but Harriet can't recall at which different store (Foot 
  Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
  Can you help these two figure out the order in which Harriet bought each 
  pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
  
  Determine: Order - Shoes - CPStore 
  ""

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object ArchFriends extends CPModel with App {

  // 
  // Decomposition of inverse constraint
  // 
  // Channel of positions of x and y:
  //    j == x(i) <=> y(j) == i
  // 
  // Note: This requires the domain 0..n-1
  //
  def inverse(cp: CPSolver, x: Array[CPIntVar], y: Array[CPIntVar]) {
    val len = x.length
    for (
      i <- 0 until len;
      j <- 0 until len
    ) {
      cp.add((y(j) === i) == (x(i) === j))
    }
  }

  // Convenient function which returns y (for presentation)
  def inverse2(cp: CPSolver, x: Array[CPIntVar]): Array[CPIntVar] = {
    val y = Array.fill(x.length)(CPIntVar(x(0).min to x(0).max)(cp))
    inverse(cp, x, y)
    y
  }

  //
  // data
  //
  val n = 4

  //
  // variables
  //
  val shoes = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(ecru_espadrilles, fuchsia_flats, purple_pumps, suede_sandals) = shoes
  // for output
  val shoesStr = Array("Ecru Espadrilles", "Fuchsia Flats", "Purple Pumps", "Suede Sandals")
  val shoesInv = inverse2(solver, shoes)

  val shops = Array.fill(n)(CPIntVar(0 to n - 1))
  val Array(foot_farm, heels_in_a_handcart, the_shoe_palace, tootsies) = shops
  // for output
  val shopsStr = Array("Foot Farm", "Heels in a Handcart", "The Shoe Palace", "Tootsies")
  val shopsInv = inverse2(solver, shops)

  //
  // constraints
  //
  var numSols = 0

  add(allDifferent(shoes), Strong)
  add(allDifferent(shops), Strong)

  // 1. Harriet bought fuchsia flats at Heels in a Handcart.
  add(fuchsia_flats == heels_in_a_handcart)

  // 2. The store she visited just after buying her purple
  //    pumps was not Tootsies.
  add(purple_pumps + 1 != tootsies)

  //  3. The Foot Farm was Harriet's second stop.
  add(foot_farm == 1)

  // 4. Two stops after leaving The Shoe Place, Harriet 
  //    bought her suede sandals.
  add(the_shoe_palace + 2 == suede_sandals)

  search { binaryMaxDegree(shoes ++ shops) }

  onSolution {
    println("Shops: " + shops.mkString(" "))
    println("Shoes: " + shoes.mkString(" "))
    println()
    println("Shops: " + shopsInv.map(s => shopsStr(s.value)).mkString(", "))
    println("Shoes: " + shoesInv.map(s => shoesStr(s.value)).mkString(", "))
    println()
    println((0 until n).
      map(s => Array(shopsStr(shopsInv(s).value), shoesStr(shoesInv(s).value)).mkString(": ")).mkString("\n"))

    numSols += 1
  }

  val stats = start()

  println("\nIt was " + numSols + " solutions.")
  println(stats)
}
