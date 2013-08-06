#!/usr/bin/ruby
#
# Survo Puzzle in Gecode/R
#
# http://en.wikipedia.org/wiki/Survo_Puzzle
# """
# Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
# by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
# Survo system which is a general environment for statistical computing and 
# related areas.
# 
# In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
# that each of these numbers appears only once and their row and column sums are 
# equal to integers given on the bottom and the right side of the table. 
# Often some of the integers are given readily in the table in order to 
# guarantee uniqueness of the solution and/or for making the task easier.
# """
# 
# See also
# http://www.survo.fi/english/index.html
# http://www.survo.fi/puzzles/index.html
#
# References:
# Mustonen, S. (2006b). "On certain cross sum puzzles", http://www.survo.fi/papers/puzzles.pdf 
# Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles.", http://www.survo.fi/papers/enum_survo_puzzles.pdf 
# Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles", http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
# R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R

#
# Compare with my other Survo Puzzle models
#
# - MiniZinc: http://www.hakank.org/minizinc/survo_puzzle.mzn
# - JaCoP: http://www.hakank.org/JaCoP/SurvoPuzzle.java
# - Choco: http://www.hakank.org/choco/SurvoPuzzle.java

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
require 'enumerator'

STDOUT.sync = true


class Array

  def print_matrix(rowsums, colsums)
    r = rowsums.length #  number of rows 
    c = colsums.length # number of columns
    
    r.times{|i|
      c.times{|j|
        printf "%3d ", self[i*c+j]
      }
      printf " = %3d ", rowsums[i]
      puts
    }
    
    colsums.map{|e| printf "%2s= ",""}
    puts
    colsums.map{|e| printf "%3d ", e}
    
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
class SurvoPuzzle
  
  include Gecode::Mixin
  
  def initialize(clues, rowsums, colsums)
    
    r = rowsums.length #  number of rows 
    c = colsums.length # number of columns

    #
    # the solution matrix
    #
    x_is_an int_var_matrix(r, c, 1..r*c)

    x.must_be.distinct

    #
    # values in clues with values > 0 is copied straight off
    # to x
    #
    r.times{|i| 
      c.times{|j|
          x[i,j].must == clues[i][j] if clues[i][j] > 0
      }
    }

    # check row sums
    r.times{|i| x[i,0..c-1].sum.must == rowsums[i] }

    # check column sums
    # Note the transpose of the matrix x
    c.times{|j| x.transpose[j,0..r-1].sum.must == colsums[j] }


    branch_on x, :variable => :smallest_size, :value => :min


  end # end initialize
  
  
end # end class

file = ARGV[0] || ""

#
# Default problem:
# Survo puzzle 126/2008 (25) #363-33148
# From http://www.survo.fi/puzzles/280708.txt
#
rowsums = [32,79,60]
colsums = [24,22,43,35,39,8]
clues = [
         [ 0, 4, 0, 0, 0, 0,32],
         [12, 0, 0,16,17, 0,79],
         [ 0, 0,15, 0, 0, 2,60],
        ]

if file.length > 0 and File::exist?(file) then
  t_rowsums = []
  t_clues = []
  last_row = []
  open(file).each{|line|
    next if line =~ /^(%|#)/ # comment
    next if line =~ /^\s*$/
    next if line =~ /^\s*A/
    line.chomp!
    line.gsub!('_','')
    line.sub!(/^\s+/,"")
    row = line.split(/\s+/)
    row = row.map{|e| e == '*' ? 0 : e; e = e.to_i}
    last_row = row
    t_rowsums << row[-1]
    row = row[1,row.size-2]
    
    t_clues << row.to_a

  }

  t_colsums = last_row
  t_rowsums = t_rowsums[0..-2]
  t_clues = t_clues[0..t_clues.size-1]

  clues = t_clues
  rowsums = t_rowsums
  colsums = t_colsums

end

r = rowsums.length #  number of rows 
c = colsums.length # number of columns

survo_puzzle = SurvoPuzzle.new(clues, rowsums, colsums)
num_solutions = 0
survo_puzzle.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}";
  x = s.x.values
  # x is an Array
  x.print_matrix(rowsums, colsums)
  print "\n\nStatistics:\n"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"
