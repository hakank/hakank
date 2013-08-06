#!/usr/bin/ruby
#
# Coins grid problem in Gecode/R
#
# Problem from 
# Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
# http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
#
# """
# In a quadratic grid (or a larger chessboard) with 31x31 cells, one should 
# place coins in such a way that the following conditions are fulfilled:
#    1. In each row exactly 14 coins must be placed.
#    2. In each column exactly 14 coins must be placed.
#    3. The sum of the quadratic horizontal distance from the main diagonal 
#       of all cells containing a coin must be as small as possible.
#    4. In each cell at most one coin can be placed.
#
# The description says to place 14x31 = 434 coins on the chessboard each 
# row containing 14 coins and each column also containing 14 coins.
# """
#
#
# This model is quite slow (cf Gecode/flatzinc below): The problem
# for n = 8, c = 4 takes about 11 seconds! This is about the same
# time as Gecode/flatzinc for a MiniZinc model.
# 
#
# Compare with the following model
#
# - LPL model (LPL is Hurlimann's constraint solving system)
#   http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
#   which solves the problem very fast.
#
# Or my other models:
# 
# - MiniZinc model: http://www.hakank.org/minizinc/coins_grid.mzn
#   The linear programming solvers, e.g ECLiPSe/eplex and MiniZinc/mip
#   are very fast. Gecode/flatzinc, ECLiPSe/ic, MiniZinc/minizinc are
#   slow.
#
# - JaCoP model: http://www.hakank.org/JaCoP/CoinsGrid.java
# 
# - Choco model: http:// www.hakank.org/choco/CoinsGrid.java 
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
  # Sums all the elements in the array using #+ .
  #
  def sum
    inject{ |sum, element| sum + element }
  end

  # cross product:
  # i.e. sum(i in 0..n-1) (x[i]*y[i])
  def sum2(y)
    zip(y).map{|a,b| a*b}.inject{|sum,c| sum+c}
  end

  def print_matrix
    size = Math.sqrt(self.length).to_i
    for i in 0..size-1 do
      for j in 0..size-1 do
        print self[i*size+j], " "
      end
      puts
    end

  end


end

class Vector

  # sum a row or column in a matrix
  def sum_row
    inject([]){ |arr, e| arr << e }.sum
  end


end

class Matrix

  #
  # sums all elements in the matrix
  #
  def matrix_sum
    s = []
    for i in 0..self.row_size-1 do
      for j in 0..self.column_size-1 do
        s << self[i,j]
      end
    end

    return s.sum

  end # end matrix_sum

end


class Gecode::Util::EnumMatrix

  #
  # sums all elements in the matrix
  #
  def sum_matrix
    inject([]){|arr,e| arr << e}.sum
  end

end


class Coins

  include Gecode::Mixin

  #
  # The original problem from Hurlimann's paper has
  # the following values:
  #   n = 31 # the grid size: n x n
  #   c = 14 # number of coins per row/column
  #
  def initialize(n = 6, c = 3)
 
    #
    # the grid
    #
    x_is_an int_var_matrix(n, n, 0..1)

    #
    # all rows and columns must have c coins placed
    #
    x.row_size.times do |i|
      x.row(i).sum_row.must == c
      x.column(i).sum_row.must == c
    end
    
    #
    # the sum of quadratic horizontal distance
    #
    z_is_an int_var

    s = []
    for i in 0..x.row_size-1 do
      for j in 0..x.column_size-1 do
        s << x[i,j]*(i-j).abs*(i-j).abs
      end
    end

    z.must == s.sum

    # branch_on x, :variable => :smallest_size, :value => :min
    # branch_on z, :variable => :smallest_size, :value => :min

    # This is better
    branch_on x, :variable => :largest_degree, :value => :min
    branch_on z, :variable => :largest_degree, :value => :min


  end

end


n = (ARGV[0] || 8).to_i
c = (ARGV[1] || 3).to_i 
puts "n: #{n} c: #{c}"
coins = Coins.new(n,c)

coins.minimize!(:z)

#
# this is better if we want to see the 
# development of the model, e.g. compare with
# the behaviour of Gecode/flatzinc (MiniZinc)
#
# coins.optimize! do |model, best_so_far|
#  model.z.must < best_so_far.z.value
#  puts "\nz: #{model.z.value}"
#  x = model.x.values
#  x.print_matrix
#end


num_solutions = 1
coins.each_solution{|s| 
  puts "\nSolution ##{num_solutions}"

  z = s.z.value
  puts "z: #{z}"
  x = s.x.values

  #
  # print the solution
  #
  x.print_matrix

  num_solutions += 1

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nThere were #{num_solutions} solutions"


