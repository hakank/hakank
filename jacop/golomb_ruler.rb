#!/usr/bin/ruby
#
#
# Golomb ruler in Gecode/R
#
# CSPLib problem number 6
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob006/index.html
# """
# These problems are said to have many practical applications including sensor placements for 
# x-ray crystallography and radio astronomy. A Golomb ruler may be defined as a set of m 
# integers 0 = a_1 < a_2 < ... < a_m such that the m(m-1)/2 differences 
# a_j - a_i, 1 <= i < j <= m are distinct. Such a ruler is said to contain m marks 
# and is of length a_m. The objective is to find optimal (minimum length) or near 
# optimal rulers.
# 
# Note that a symmetry can be removed by adding the constraint that 
# a_2 - a_1 < a_m - a_{m-1}, the first difference is less than the last.
# 
# There exist several interesting generalizations of the problem which have received 
# attention like modular Golomb rulers (differences are all distinct mod a given base), 
# disjoint Golomb rulers, Golomb rectangles (the 2-dimensional generalization of Golomb 
# rulers), and difference triangle sets (sets of rulers with no common difference). 
# """
#
# References:
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob006/refs.html
#
#
# Cf Gecode's model http://www.gecode.org/gecode-doc-latest/golomb-ruler_8cc-source.html
#
 
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#

#
# Comparing some different strategies for :variable and :value , with just
# one run. 
#
# Note: the time is for running the specific model, not counting the time 
# for startup of the program.
#
# The winner is :variable => :largest_size and :value =>: split_min 
#               which for m = 10 took about 1 second.
# 
# The listing below is
#    variable strategy : value strategy  -> running time
#
# largest_size:split_min -> 1.114403
# smallest_min:split_min -> 5.275035
# smallest_min_regret:split_min -> 5.289433
# none:split_min -> 5.320299
# smallest_degree:split_min -> 5.344738
# smallest_degree:min -> 5.551923
# none:min -> 5.556326
# smallest_min:min -> 5.567722
# largest_max_regret:split_min -> 5.621116
# smallest_min_regret:min -> 5.657271
# largest_max_regret:min -> 5.783116
# largest_size:min -> 6.067107
# smallest_size:split_min -> 7.006949
# largest_min:split_min -> 8.229713
# smallest_size:min -> 11.800805
# largest_min:split_max -> 14.558584
# largest_min:min -> 14.736679
# largest_min:max -> 17.301876
# largest_size:split_max -> 17.564312
# largest_min:med -> 23.655615
# largest_size:max -> 26.835556
# smallest_min:split_max -> 60.566207
# largest_max_regret:split_max -> 60.605995
# smallest_min_regret:split_max -> 60.79834
# smallest_degree:split_max -> 60.948791
# none:split_max -> 63.229398
# smallest_size:split_max -> 80.155812
# smallest_min_regret:max -> 84.988649
# none:max -> 85.208536
# smallest_degree:max -> 85.541162
# largest_max_regret:max -> 85.64104
# smallest_min:max -> 85.859321
# smallest_size:max -> 93.474409
# none:med -> 102.263753
# largest_max_regret:med -> 103.352322
# smallest_degree:med -> 103.688327
# smallest_min:med -> 103.78953
# smallest_size:med -> 131.829423
# largest_size:med -> 160.26752
# smallest_min_regret:med -> 181.760703


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Array

  #
  # print the solution
  #
  def print_matrix
    size = Math.sqrt(self.length).to_i
    for i in 0..size-1 do
      for j in 0..size-1 do
         print self[i*size+j], " "
      end
      puts
   end

  end

  #
  # sum an array
  #
  def sum
    inject{ |sum, element| sum + element }
  end


end # end Array

class Golomb

  include Gecode::Mixin

  def initialize(m=5, b_var = :smallest_size, b_val = :min)

    n = m*m
    mark_is_an int_var_array(m, 0..n)
    # mark.must_be.distinct(:strength => :bounds)

    differences_is_an int_var_array((m*(m-1)) / 2, 0..n)
    differences.must_be.distinct(:strength => :bounds)
    
    last_mark_is_an int_var(0..n)
    last_mark.must.equal(mark[m-1], :strength => :bounds)

    c = 0;
    (0..m-1).each{|i|
      (i+1..m-1).each{|j|
        differences[c].must.equal((mark[j] - mark[i]), :strength => :bounds)
        c += 1
      }
    }

    mark[0].must.equal(0, :strength => :bounds)

    (0..m-3).each{|i|
      mark[i].must_be.less(mark[i+1], :strength => :bounds)
    }

    (mark[1] - mark[0]).must_be.less((mark[m-1] - mark[m-2]), :strength => :bounds)

    # branch_on mark, :variable => :none, :value => :min
    # branch_on last_mark, :variable => :smallest_size, :value => :split_min
    # branch_on differences, :variable => :none, :value => :min

    branch_on mark, :variable => b_var, :value => b_val
    branch_on last_mark, :variable => b_var, :value => b_val
    # branch_on last_mark, :variable => :smallest_size, :value => :min
    branch_on differences, :variable => b_var, :value => b_val


  end # end initialize


end # end class

#
# :largest_size and :split_min won the game!
#

# These lists is for the benchmark (see above), which took about 30 minutes 
# to run all the combinations...
branch_variables = [:largest_size] # [:none, :smallest_degree, :smallest_min_regret, :largest_max_regret,:largest_min, :largest_size,:smallest_min, :smallest_size]
branch_values = [:split_min] # [:split_min,:min, :max, :med, :split_max]

min_b_var = nil
min_b_val = nil
min_time = 10000
h = {}
branch_variables.each{|b_var|
branch_values.each{|b_val|
m = (ARGV[0] || 10).to_i

puts "\nchecking: m:#{m} b_var:#{b_var} b_val:#{b_val}"
num_solutions = 0
start_time = Time.now

golomb = Golomb.new(m, b_var, b_val)
golomb.minimize! :last_mark
golomb.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts "mark:#{s.mark.values.join(' ')} last_mark:#{s.last_mark.value}"
  # puts "differences:#{s.differences.values.join(' ')}"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}
end_time = Time.now
diff_time = end_time - start_time
h["#{b_var}:#{b_val}"] = diff_time
if diff_time < min_time  then
  min_time = diff_time
  min_b_var = b_var
  min_b_val = b_val
end

puts "\nNumber of solutions: #{num_solutions}  Time: #{diff_time}"
  }
}

puts "\n\nBest: b_var:#{min_b_var} b_val: #{min_b_val} min_time:#{min_time}"
puts "\nThe strategies:"
h.sort{|a,b| a[1] <=> b[1]}.each{|k,v| puts "#{k} -> #{v}"}
