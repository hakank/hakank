#!/usr/bin/ruby
#
#
# De Bruijn sequences in Gecode/R
# Both "classical" and "arbitrary".
#
# Compare with 

#  - MiniZinc model http://www.hakank.org/minizinc/debruijn_binaru.mzn.
#
#  - the web based programs:
#    http://www.hakank.org/comb/debruijn.cgi     (classic)
#    http://www.hakank.org/comb/debruijn_arb.cgi ("arbitrary")
#
#  - JaCoP: http://www.hakank.org/jacop/DeBruijn.java
#  - Choco:#  http://www.hakank.org/choco/DeBruijn.java
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
require 'enumerator'
require 'matrix'
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
  
  def toNum(base=2)
    self.inject{ |result, variable| variable + result*base}
  end

  #
  # sum an array
  #
  def sum
    inject{ |sum, element| sum + element }
  end


 
end # end Array

#
# The problem
#
class DeBruijn
  
  include Gecode::Mixin

 
  def initialize(base = 2, n = 4, m = base**n)
    
    puts "base: #{base} n:#{n} m=#{m} base**n-1:#{base**n-1}"

    # the integers
    x_is_an int_var_array(m, 0..base**n-1)

    # "base" conversion of integers
    binary_is_an int_var_matrix(m, n, 0..base-1)

    # first element in binary matrix
    bin_code_is_an int_var_array(m, 0..base-1)

    # all integers must be different
    x.must_be.distinct

    for i in 0..m-1 do
      # convert integer in x <-> the "binary" row in binary
      x[i].must == binary[i,0..n-1].toNum(base)

      # converts binary -> bin_code (i.e. first element in each binary "row")
      bin_code[i].must == binary[i,0]
    end

    # the de Bruijn condition
    for i in 1..m-1 do 
      for j in 1..n-1 do
        binary[i-1,j].must == binary[i, j-1]
      end
    end

    # ... and around the corner
    for j in 1..n-1 do
      binary[m-1,j].must == binary[0,j-1]
    end

    # Symmetry breaking. 
    # The minimum element should be placed first
    x[0].must == x.min

    # Global cardinality count (gcc): 
    # How many of each digits is in bin_code?
    gcc_is_an int_var_array(base, 0..m)
    for i in 0..base-1 do
      gcc[i].must == bin_code.count(i)
    end

    # This works but is not very interesting (it was used as a test in
    # the MiniZinc model).
    # If m is base^n then all elements in gcc must be the same
    # if m == base**n then 
    #  gcc.must.equal
    # end

    branch_on x        , :variable => :smallest_size, :value => :min
    branch_on binary   , :variable => :smallest_size, :value => :min
    branch_on bin_code , :variable => :smallest_size, :value => :min
    branch_on gcc      , :variable => :smallest_size, :value => :min
    
  end # end initialize
  
  
end # end class


base  = (ARGV[0] || 2).to_i
n     = (ARGV[1] || 4).to_i
m     = (ARGV[2] || base**n).to_i 
n_sol = (ARGV[3] || 0).to_i
debruijn = DeBruijn.new(base, n, m)
num_solutions = 0
debruijn.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}";

  x = s.x.values
  puts "x: #{x.join(' ')}"

  binary = s.binary.values
  puts "binary: #{binary.join(' ')}"
  bin_code = s.bin_code.values
  puts "bin_code: #{bin_code.join(' ')}"
  puts "gcc: #{s.gcc.values.join(' ')}"

  for i in 0..m-1 do
    for j in 0..n-1 do
      # binary is now an Array
      print binary[i*n+j], " "
    end
    puts "  (x:#{x[i]} bin_code:#{bin_code[i]})"
  end

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

  break if n_sol > 0 and num_solutions >= n_sol

}

puts "\nNumber of solutions: #{num_solutions}"




