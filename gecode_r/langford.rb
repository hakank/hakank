#!/usr/bin/ruby
#
# Langford number's problem in Gecode/R
#
# Langford's number problem (CSP lib problem 24)
# http://www.csplib.org/prob/prob024/
# """
# Arrange 2 sets of positive integers 1..k to a sequence,
# such that, following the first occurence of an integer i, 
# each subsequent occurrence of i, appears i+1 indices later
# than the last. 
# For example, for k=4, a solution would be 41312432
# """
# 
# * John E. Miller: Langford's Problem
#   http://www.lclark.edu/~miller/langford.html
# 
# * Encyclopedia of Integer Sequences for the number of solutions for each k
#   http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
#
# * MiniZinc model http://www.hakank.org/minizinc/langford2.mzn

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Langford

  include Gecode::Mixin

  def initialize(k = 4)

    # positions where to place a numbers
    position_is_an int_var_array(k*2+1, 0..2*k)
    # the solution
    x_is_an int_var_array(k*2+1, 0..k)

    position.must_be.distinct

    # "Channel" between the positions and the solution.
    # The first 1..k positions is where to put the first number (in the pair)
    # The second k+1..2*k where to put the second number.
    (1..k).each{|i|
      position[i+k].must == position[i] + i + 1
      x[position[i]].must == i
      x[position[k+i]].must == i
      x.count(i).must == 2 # implicit constraint which makes is slightly faster
    }

    

    # since we are 0-based we fix index 0 
    x[0].must == 0
    position[0].must == 0

    # symmetry breaking
    x[1].must < x[2*k]

    # Note: the branch on position is placed first, since it's much faster
    #       that if it's placed second
    branch_on position, :variable => :smallest_size, :value => :min 
    branch_on x       , :variable => :smallest_size, :value => :min


  end # end initialize


end # end class

k     = (ARGV[0] || 4).to_i
n_sol = (ARGV[1] || 0).to_i

langford = Langford.new(k)
num_solutions = 0
langford.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  # position = s.position.values
  # puts "position: #{position[1..position.length].join(' ')}"
  x = s.x.values
  puts "sol     : #{x[1..x.length].join(' ')}"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}
  break if n_sol > 0 and num_solutions >= n_sol

}

puts "\nNumber of solutions: #{num_solutions}"




