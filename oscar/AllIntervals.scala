package oscar.examples.cp.hakank

import oscar.cp.modeling._

import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*
  All intervals problem in Oscar.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """
  
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object AllIntervals extends CPModel with App {

  // Data

  val n = if (args.length > 0) args(0).toInt else 11;
  val num_to_show = if (args.length > 1) args(1).toInt else 1;

  println("n: " + n)

  // Variables

  val x = Array.fill(n)(CPIntVar(0 to n - 1))
  val diffs = Array.fill(n - 1)(CPIntVar(1 to n - 1))

  // Constraints

  add(allDifferent(diffs), Strong)
  add(allDifferent(x), Strong)

  for (k <- 0 until n - 1) {
    add(diffs(k) == (x(k + 1) - (x(k))).abs())
  }

  // Symmetry breaking
  add(x(0) < x(n - 1))
  add(diffs(0) < diffs(1))

  search { binaryStatic(x) }

  onSolution {
    print("x:" + x.mkString(""))
    print("  diffs:" + diffs.mkString(""))
    println()
  }

  val stats = start(nSols = num_to_show)
  println(stats)
}
