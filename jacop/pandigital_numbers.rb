#!/usr/bin/ruby
#
# Pandigital numbers (any base <= 10) in Gecode/R
#
# 
# From 
# Albert H. Beiler "Recreations in the Theory of Numbers", quoted from
# http://www.worldofnumbers.com/ninedig1.htm
# """
# [ Chapter VIII : Digits - and the magic of 9 ]
# [ I found the same exposé in Shakuntala Devi's book 
#   "Figuring : The Joy of Numbers" ]
#
# The following curious table shows how to arrange the 9 digits so that 
# the product of 2 groups is equal to a number represented by the 
# remaining digits."
#
#   12 x 483 = 5796 
#   42 x 138 = 5796 
#   18 x 297 = 5346 
#   27 x 198 = 5346 
#   39 x 186 = 7254 
#   48 x 159 = 7632 
#   28 x 157 = 4396 
#   4 x 1738 = 6952 
#   4 x 1963 = 7852
# """
#
# See also
# 
# * MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
# """
# A number is said to be pandigital if it contains each of the digits 
# from 0 to 9 (and whose leading digit must be nonzero). However, 
# "zeroless" pandigital quantities contain the digits 1 through 9. 
# Sometimes exclusivity is also required so that each digit is 
# restricted to appear exactly once.
# """
#
# * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number
#
# Also see my MiniZinc model http://www.hakank.org/minizinc/pandigital_numbers.mzn
#


#
# Notes: 
# Since the max integer value in Gecode is 2147483648 (2**31) the maximum
# base this model can handle is 10.
#
# Here is the number of solutions for base from 2 to 10 and start 0 or 1
#   base  2, start 0:  0
#   base  2, start 1:  0
#   base  3, start 0:  0
#   base  3, start 1:  0
#   base  4, start 0:  0
#   base  4, start 1:  0
#   base  5, start 0:  0
#   base  5, start 1:  1
#   base  6, start 0:  0
#   base  6, start 1:  1
#   base  7, start 0:  2
#   base  7, start 1:  2
#   base  8, start 0:  4
#   base  8, start 1:  4
#   base  9, start 0: 10
#   base  9, start 1:  6
#   base 10, start 0: 22
#   base 10, start 1:  9
#
# 

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

  def toNum(base=10)
    self.inject{ |result, variable| variable + result*base}
  end

  # not used
  def check(len1, len2, num1, num2, res,base,x_len)
    # puts "num1:#{0..len1-1} num2:#{len1..len1+len2-1} res:#{len1+len2..x_len}"

    # num1
    self[0..len1-1].toNum(base).must == num1
    
    # num2 
    self[len1..len1+len2-1].toNum(base).must == num2
    
    # res
    self[len1+len2..x_len].toNum(base).must == res

  end
  
  
end # end Array

class PanDigital
  
  include Gecode::Mixin

  def initialize(base=10, start=1, len1=1, len2=1)
    
    max_d = base-1
    x_len = max_d+1-start

    x_is_an int_var_array(x_len, start..max_d)

    x.must_be.distinct

    # max value of each number, e.g. 987654321
    max_num = (start..base-1).to_a.inject{|s,e| e+base*s} / 2

    num1_is_an int_var(0..max_num)
    num2_is_an int_var(0..max_num)
    res_is_an int_var(0..max_num)

    # num1
    x[0..len1-1].toNum(base).must == num1
     
    # num2 
    x[len1..len1+len2-1].toNum(base).must == num2
     
    # res
    x[len1+len2..x_len].toNum(base).must == res


    # no number must start with 0
    x[0].must > 0
    x[len1].must > 0
    x[len1+len2].must > 0

    # symmetry breaking
    num1.must <= num2

    (num1*num2).must == res
    # num1.must > 0
    # num2.must > 0

    branch_on x   , :variable => :smallest_size, :value => :min
    branch_on num1, :variable => :smallest_size, :value => :min
    branch_on num2, :variable => :smallest_size, :value => :min
    branch_on res , :variable => :smallest_size, :value => :min

  end # end initialize

end # end class

base = (ARGV[0] || 10).to_i
start = (ARGV[1] || 1).to_i
n_sol = (ARGV[2] || 0).to_i


# uncomment these two lines (and the corresponding end lines) for
# the solution of all bases
#for base in 2..10 do
#  for start in 0..1 do

x_len = base-1 + 1-start
puts "base: #{base} start:#{start}"
h = Hash.new(0)
num_solutions = 0
for len1 in 1..1+(x_len / 3) do
  for len2 in 1..1+(x_len / 3) do
    
    if x_len > len1 + len2 then
      # puts "base:#{base} x_len:#{x_len} start:#{start} len1:#{len1} len2:#{len2}"
      pandigital = PanDigital.new(base, start, len1, len2)
      pandigital.each_solution{|s| 
        num_solutions += 1
        puts "Solution ##{num_solutions}";
        x = s.x.values
        # puts "x: #{x.join(' ')}"
        puts "#{s.num1.value} * #{s.num2.value} = #{s.res.value} (base 10)"
        join_str = base > 10 ? ' ' : ''
        if base != 10 then
          puts "#{x[0..len1-1].join(join_str)} * #{x[len1..len1+len2-1].join(join_str)} = #{x[len1+len2..x_len].join(join_str)} (base #{base})"
        end
        
        h[s.res.value] += 1
          
        # s.search_stats.each{|w,v| puts "#{w}: #{v}"}
        puts
        
        break if n_sol > 0 and num_solutions >= n_sol
        
      }
    end
  end
end

puts "Number of solutions for base #{base} and start #{start}: #{num_solutions}"

#end
#end



# print "all results: "
# p h


