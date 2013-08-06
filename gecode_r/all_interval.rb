#!/usr/bin/ruby
#
# All interval problem in Gecode/R
# 
# CSPLib problem number 7
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
# """
# Given the twelve standard pitch-classes (c, c#, d, ...), represented by 
# numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
# once and in which the musical intervals between neighbouring notes cover 
# the full set of intervals from the minor second (1 semitone) to the major 
# seventh (11 semitones). That is, for each of the intervals, there is a 
# pair of neigbhouring pitch-classes in the series, between which this 
# interval appears. The problem of finding such a series can be easily 
# formulated as an instance of a more general arithmetic problem on Z_n, 
# the set of integer residues modulo n. Given n in N, find a vector 
# s = (s_1, ..., s_n), such that (i) s is a permutation of 
# Z_n = {0,1,...,n-1}; and (ii) the interval vector 
# v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
# Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
# called an all-interval series of size n; the problem of finding such 
# a series is the all-interval series problem of size n. We may also be 
# interested in finding all possible series of a given size. 
# """

#
# Also see the MiniZinc model http://www.hakank.org/minzinc/all_interval.mzn
#

#
# This Gecode/R model below is a quite simple and direct approach to 
# the problem.
#
# For n = 12, all 1328 solutions (no symmetry breaking) takes about 40 seconds.
# With symmetry breaking: 332 solutions in 12 seconds
#

# ....
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class AllInterval

  include Gecode::Mixin

  def initialize(n = 12)

    x_is_an int_var_array(n, 1..n)
    diffs_is_an int_var_array(n-1, 1..n-1)

    x.must_be.distinct(:strength => :bounds)
    diffs.must_be.distinct(:strength => :bounds)

    (0..n-2).each{|k|
      diffs[k].must.equal((x[k+1] - x[k]).abs, :strength => :bounds)
    }

    # symmetry breaking: for n=12 -> 332 solutions in 12 seconds
    x[0].must < x[1]
    diffs[0].must > diffs[n-2]

    # another symmetry breaking: for n=12 -> 463 solutions in 16 seconds
    # x[0].must < x[n-1]
    # diffs[0].must < diffs[1]


    branch_on x    , :variable => :smallest_size, :value => :min
    branch_on diffs, :variable => :smallest_size, :value => :min


  end # end initialize

end # end class

n = (ARGV[0] || 12).to_i
n_sols = (ARGV[1] || 0).to_i

puts "n: #{n}"

all_interval = AllInterval.new(n)
num_solutions = 0
all_interval.each_solution{|s| 
  num_solutions += 1
  # puts "\nSolution ##{num_solutions}\n";
  puts "x: [#{s.x.values.join(',')}]  diffs: [#{s.diffs.values.join(',')}]"

  # s.search_stats.each{|w,v| puts "#{w}: #{v}"}

  break if n_sols > 0 and num_solutions >= n_sols

}

puts "\nNumber of solutions: #{num_solutions}"



