#!/usr/bin/ruby
# 
# Set partition problem in Gecode/R
#
# Problem formulation from
#   http://www.koalog.com/resources/samples/PartitionProblem.java.html
# """
#  This is a partition problem.
#  Given the set S = {1, 2, ..., n}, 
#  it consists in finding two sets A and B such that:
#  
#    A U B = S,
#    |A| = |B|,
#    sum(A) = sum(B),
#    sum_squares(A) = sum_squares(B)
# 
# """
#
#
#
# This model somewhat generaliz the problem by allowing for any number
# of sets (num_sets below). The symmetry breaking is for the two-case only, though.
#
#
# Related:
# * http://en.wikipedia.org/wiki/Partition_problem
#
#
# Cf 
# * MiniZinc model: http://www.hakank.org/minizinc/set_partition.mzn
# * Gecode model: http://www.gecode.org/gecode-doc-latest/classPartition.html
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class SetPartition

  include Gecode::Mixin

  #
  # partition_set(s, universe)
  # partitions an set_var_array 's' in disjoint sets 
  # given the set universe 'universe'
  #
  #
  def partition_set(s, universe)
    all_disjoint(s) 
    s.union.size.must == universe.entries.length
  end

  #
  # all_distjoint(s)
  # ensures that all sets in s are disjoint
  #
  # s: set_var_array
  #
  def all_disjoint(s)
    n = s.length
    n.times{|i|
      (i+1..n-1).each{|j|
        s[i].must_be.disjoint_with s[j]
      }
    }
  end


  #
  # initialize
  #
  def initialize(n = 16, num_sets = 2) 


    a_is_an set_var_array(num_sets, [], 1..n)

    partition_set(a, 1..n) # partition the sets

    # for the squares
    h = Hash.new
    (1..n).each{|i| h[i] = i**2}

    (1..num_sets-1).each{|i|
      a[i].size.must == a[i-1].size # |a| == |b|
      a[i].sum.must  == a[i-1].sum # plain sum
      # sum squared
      a[i].sum(:substitutions => h).must  == a[i-1].sum(:substitutions => h) 
    }

    # symmetry breaking 
    # the element 1 must be in the first set
    a[0].must_be.superset_of 1..1

    # calculate the sums and squared sums
    s_sum_is_an int_var
    s_sum.must == a[0].sum

    s_sum_squared_is_an int_var
    s_sum_squared.must == a[0].sum(:substitutions => h) 


    branch_on a, :variable => :smallest_unknown, :value => :max
    branch_on s_sum
    branch_on s_sum_squared

  end # end initialize


end # end class

n = (ARGV[0] || 16).to_i
num_sets = (ARGV[1] || 2).to_i
n_sol = (ARGV[2] || 0).to_i

#for n in 2..40 do
set_partition = SetPartition.new(n, num_sets)
num_solutions = 0
set_partition.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts s.a.values.map{|v| "{#{v.to_a.join(', ')}} = #{s.s_sum.value}, #{s.s_sum_squared.value}"}.join("\n")
  
  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

  break if n_sol > 0 and num_solutions >= n_sol
  
}

puts "\nNumber of solutions for n=#{n}: #{num_solutions}"
# end



