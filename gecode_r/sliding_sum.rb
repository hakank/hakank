#!/usr/bin/ruby
#
#
# Global constraint sliding_sum in Gecode/R
#
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Array

  #
  # sum an array
  #
  def sum
    inject{ |sum, element| sum + element }
  end


end # end Array

class SlidingSum

  include Gecode::Mixin

  # This implements a simple sliding sum constraint:
  # All sliding sums of length slice equals to_sum
  def sliding_sum(x, slice, low, high)
    (slice-1..x.length-1).each{|i|
      wrap_enum(x[i-slice+1..i]).sum.must >= low
      wrap_enum(x[i-slice+1..i]).sum.must <= high
    }
  end

  # Convenience method: must sun to exactly to_sum
  def sliding_sum_equal(x, slice, to_sum)
     sliding_sum(x, slice, to_sum, to_sum)
  end


  # a_size: array size
  # s_len: length of the slice (window)
  # low: min value
  # high: max value
  def initialize(a_size, s_len, low, high=low)

    x_is_an int_var_array(a_size, 0..a_size-1)

    # sliding_sum_equal(x,slice, low)
    sliding_sum(x, s_len, low, high)


    branch_on x, :variable => :largest_degree, :value => :max
    # branch_on x, :variable => :smallest_size, :value => :min 

  end # end initialize


end # end class


a_size = (ARGV[0] || 10).to_i
s_len  = (ARGV[1] || 3).to_i
low    = (ARGV[2] || 10).to_i
high   = (ARGV[3] || 10).to_i

sliding_sum = SlidingSum.new(a_size, s_len, low, high)
num_solutions = 0
sliding_sum.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts s.x.values.join(" ")

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




