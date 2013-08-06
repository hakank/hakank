#!/usr/bin/ruby
#
#
# Steiner triplets in Gecode/R
#
# A standard constraint programming problem:
# Find a set of triplet of numbers from 1 to n such that
# any two (different) triplets have at most one element in common.
# 
# See 
# - http://mathworld.wolfram.com/SteinerTripleSystem.html
# - http://en.wikipedia.org/wiki/Steiner_system

#
# Note: This model solves for n = 7 and 9 quite fast, in about one second.
# There is no solution for n = 11 (n must be 1 or 3 modulo 6). 
#       

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Steiner
  
include Gecode::Mixin

  def initialize(n=7)

    nb = n * (n-1) / 6 # size of the array

    if n % 6 != 1 and n % 6 != 3 then
      puts "A solution for n=#{n} is not possible! Exits..."
      exit
    end

    sets_is_an set_var_array(nb, [], 1..n)

    sets.must.at_most_share_one_element(:size => 3)

    # This is an explicit way of stating the same as the above constraint, 
    # but it really makes it faster.
    n.times{|i|
      sets[i].size.must == 3
      (i+1..n-1).each{|j|
        (sets[i].intersection(sets[j])).size.must <= 1
      }
    }


    # Symmetry breaking:
    # sets.must_be.sort # no such method: <

    branch_on sets, :variable => :smallest_unknown, :value => :min

    
  end # end initialize


end # end class

n = (ARGV[0] || 7).to_i
max_sol = (ARGV[1] || 0).to_i

steiner = Steiner.new(n)
num_solutions = 0
steiner.each_solution{|s| 
  num_solutions += 1
  puts "Solution ##{num_solutions}";
  s.sets.values.each{|v| puts "{#{v.to_a.join(', ')}}" }
  
  s.search_stats.each{|w,v| puts "#{w}: #{v}"}
  break if num_solutions >= max_sol if max_sol > 0
  puts
}

puts "\nNumber of solutions: #{num_solutions}"




