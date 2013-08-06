#!/usr/bin/ruby
#
# Devil's word in Gecode/R
#
# Translate each character in a word to ASCII value and then try
# to sum its values (either positive or negative) to a total.
# 
# E.g. "hakankkjellerstrand" and total 666 gives 359 solutions.
# Here is the first:
# +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100
#
# Also see 
#  * my CGI program "Devil's Word"
#    http://www.hakank.org/data_snooping/666.cgi
#  * the MiniZinc model http://www.hakank.org/minizinc/devils_words.mzn


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


class DevilsWord

  include Gecode::Mixin

  def initialize(arr, total)

    n = arr.length     # then length of the array
    arr_max = arr.max  # max value

    # the decision variables
    x_is_an int_var_array(n, -arr_max..arr_max)

    #
    # +/- on each value in arr
    #
    n.times{|i|  
      (x[i].must == arr[i]) | (x[i].must == (-arr[i])) 
    }
    
    x.sum.must == total
    
    branch_on x, :variable => :smallest_size, :value => :max

  end # end initialize


end # end class

word = (ARGV[0] || "hakankjellerstrand") 
total = (ARGV[1] || 666).to_i
n_sol = (ARGV[2] || 0).to_i

n = word.length
w = (0..word.length-1).map{|i| word[i].to_i}.to_a

puts "word: #{word}"
puts "array: #{w.join(' ')}"

devils_word = DevilsWord.new(w, total)
num_solutions = 0
devils_word.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  s.x.values.each{|e| print(e > 0 ? "+#{e}" : e, " ")  }
  puts

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

  break if n_sol > 0 and num_solutions >= n_sol
}

puts "\nNumber of solutions: #{num_solutions}"




