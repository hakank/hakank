#!/usr/bin/ruby
#
#
# Minesweeper in Gecode/R
#
# From gecode/examples/minesweeper.cc:
# """
# A specification is a square matrix of characters. Alphanumeric 
# characters represent the number of mines adjacent to that field. 
# Dots represent fields with an unknown number of mines adjacent to 
# it (or an actual mine).
# """
# 
# E.g.
#      "..2.3."
#      "2....."
#      "..24.3"
#      "1.34.."
#      ".....3"
#      ".3.3.."
# """
# 
# Also see 
#  
# http://www.janko.at/Raetsel/Minesweeper/index.htm
#
# http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
#
# Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/
#
# Richard Kaye's Minesweeper Pages
# http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
# Some Minesweeper Configurations
# http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
#

# Compare with my other Minesweeper models:
#
# - MiniZinc: http://www.hakank.org/minizinc/minesweeper.mzn
#
# - Choco: http://www.hakank.org/choco/MineSweeper.java
#
# - JaCoP: http://www.hakank.org/JaCoP/MineSweeper.java
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


end # end Array

class Matrix



  # 
  # sum all the neighbours of the game[i][j]
  #
  def sum_n(i,j,r1,c1)
    s = []
    for a in -1..1 do
      for b in -1..1 do
        if  i+a >= 0 and j+b >= 0 and
            i+a <=  r1 and j+b <=  c1 then
          s << self[i+a,j+b]
        end
      end
    end
    return s.sum

  end # end sum_n


end # end Matrix


class Minesweeper

  include Gecode::Mixin

  def initialize(game, r, c)

    @X = -1 # the unknowns

    # game:
    #
    # encoding for the problem (game)
    #   -1 for the unknowns, 
    #  >= 0 for number of mines in the neighbourhood
    #

    #
    # the mines to locate
    #
    mines_is_an int_var_matrix(r, c, 0..1)


    for i in 0..r-1 do
      for j in 0..c-1 do

        # sum all the number of mines in the neighbourhood of this cell
        if game[i][j] >= 0 then
          mines.sum_n(i, j, r-1, c-1).must == game[i][j] 
        end

        # important hint: there can be no mine if problem has >= 0 neighbours
        if game[i][j] > @X then
          mines[i,j].must == 0
        end

        # hint: if there is a mine it must have been an unknown
        if mines[i,j] == 1 then
          game[i][j] == @X 
        end

      end

    end

    branch_on mines, :variable => :largest_degree, :value => :max # good!
    # branch_on mines, :variable => :smallest_size, :value => :min 

  end # end initialize
    
end # end Minesweeper



#
# Default problem
#
# Problem from Gecode/examples/minesweeper.cc  problem 2
#
X = -1 # unknowns
r = 10
c = 10
game = [
        [1,X,X,2,X,2,X,2,X,X],
        [X,3,2,X,X,X,4,X,X,1],
        [X,X,X,1,3,X,X,X,4,X],
        [3,X,1,X,X,X,3,X,X,X],
        [X,2,1,X,1,X,X,3,X,2],
        [X,3,X,2,X,X,2,X,1,X],
        [2,X,X,3,2,X,X,2,X,X],
        [X,3,X,X,X,3,2,X,X,3],
        [X,X,3,X,3,3,X,X,X,X],
        [X,2,X,2,X,X,X,2,2,X]
       ]

#
# Read the problem file from command line (if any)
#
file = ARGV[0] || ""
if file != "" and File::exist?(file) then

  puts "Using problem from file: #{file}"
  g = []
  lineno = 1
  r_tmp = 0
  c_tmp = 0
  puts "Problem: "
  open(file).each{|line|
    next if /^(%|#)/ =~ line # comments

    line.chomp!
    r_tmp = line.to_i if lineno == 1
    c_tmp = line.to_i if lineno == 2

    if lineno > 2 then
      puts line.split("").join(" ")
      row = line.split("")
      # convert "." -> -1 and make'em integers
      g << row.map{|e| 
        e = -1 if e == "."
        e = e.to_i 
      }
    end

    lineno += 1
  }

  r = r_tmp
  c = c_tmp
  game = g

else 

  puts "Using the default problem."

end

minesweeper = Minesweeper.new(game,r,c)
num_solutions = 0
minesweeper.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  s.mines.values.print_matrix

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"
