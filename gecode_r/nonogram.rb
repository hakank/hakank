#!/usr/bin/ruby
#
# 
# Nonogram (a.k.a. Painting by Numbers) in Gecode/R
# 
# http://en.wikipedia.org/wiki/Nonogram
# """
# Nonograms or Paint by Numbers are picture logic puzzles in which cells in a 
# grid have to be colored or left blank according to numbers given at the 
# side of the grid to reveal a hidden picture. In this puzzle type, the 
# numbers measure how many unbroken lines of filled-in squares there are 
# in any given row or column. For example, a clue of "4 8 3" would mean 
# there are sets of four, eight, and three filled squares, in that order, 
# with at least one blank square between successive groups.
# """
#
# Also see
#   * Brunetti, Sara & Daurat, Alain (2003)
#     "An algorithm reconstructing convex lattice sets"
#     http://geodisi.u-strasbg.fr/~daurat/papiers/tomoqconv.pdf
#
#   * CSPLib problem 12 at http://www.csplib.org/
#
#   * http://www.puzzlemuseum.com/nonogram.htm
#
#   * Haskell solution:
#     http://twan.home.fmf.nl/blog/haskell/Nonograms.details
#
#   * My MiniZinc model http://www.hakank.org/minizinc/nonogram.mzn
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Nonogram 

  include Gecode::Mixin

  # 
  # parses a nonogram segment and converts to a "regexp"
  # e.g. [3,2] -> (/^0*1{3}0+1{3}0*$/ ->)
  #       [repeat(0), repeat(1,3,3), at_least_once(0),repeat(1,2,2),repeat(0)]
  # 
  def parse_regex(a, debug = false)
    puts "\nparse_regex([#{a.join(' ')})]" if debug
    r = [repeat(0)]
    puts "repeat(0)" if debug
    a.each_with_index{|e,i| 
      puts "repeat(1,#{e},#{e})" if debug
      r << repeat(1,e,e)
      if i < a.length-1 then
        r << at_least_once(0) 
        puts "at_least_once(0)" if debug
      end
    }
    puts "repeat(0)" if debug
    r << repeat(0)
    return r
  end


  def initialize(row_rules, col_rules, debug = false)

    rows = row_rules.length
    cols = col_rules.length
    
    x_is_an int_var_matrix(rows, cols, 0..1)

    # rows
    rows.times{|i| 
      x.row(i).must.match(parse_regex(row_rules[i], debug))
    }

    # columns
    cols.times{|j|
      x.column(j).must.match parse_regex(col_rules[j], debug)
    }

    # branch_on x, :variable => :smallest_size, :value => :max
    branch_on x, :variable => :none, :value => :max

  end # end initialize


end # end class


#
# Default problem
# 
# From Wikipedia http://en.wikipedia.org/wiki/Nonogram
# Animation:
# http://en.wikipedia.org/wiki/File:Paint_by_numbers_Animation.gif
#
row_rules = 
[
  [3],
  [5],
  [3,1],
  [2,1],
  [3,3,4],
  [2,2,7],
  [6,1,1],
  [4,2,2],
  [1,1],
  [3,1],
  [6],
  [2,7],
  [6,3,1],
  [1,2,2,1,1],
  [4,1,1,3],
  [4,2,2],
  [3,3,1],
  [3,3],
  [3],
  [2,1]
]
  

col_rules = 
 [
  [2],
  [1,2],
  [2,3],
  [2,3],
  [3,1,1],
  [2,1,1],
  [1,1,1,2,2],
  [1,1,3,1,3],
  [2,6,4],
  [3,3,9,1],
  [5,3,2],
  [3,1,2,2],
  [2,1,7],
  [3,3,2],
  [2,4],
  [2,1,2],
  [2,2,1],
  [2,2],
  [1],
  [1]
 ]


file = (ARGV[0] || "")

if file.length > 0 and File::exists?(file) then

  t_row_rules = []
  t_col_rules = []
  got_rows = false
  got_columns = false
  open(file).each{|line|
    next if line =~ /^(#|%)/
    next if line =~ /^\s*$/
    line.chomp!
    line.gsub(/^\s+/,'')
    line.gsub(/,\s*$/,'')
    got_rows = true and next if line =~ /^row_rules/
    got_rows = false and got_columns = true and next if line =~ /^col_rules/
    if line =~ /^\d/ then
      t = line.split(/\D+/).map{|e| e.to_i}
      if got_rows then
        t_row_rules << t
      else 
        t_col_rules << t
      end
    end
  }

  row_rules = t_row_rules
  col_rules = t_col_rules
 
end


rows = row_rules.length
cols = col_rules.length

debug = false
# debug = true

nonogram = Nonogram.new(row_rules, col_rules, debug)
num_solutions = 0
nonogram.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  x = s.x.values
  rows.times{|i|
    print "  "
    cols.times{|j|
      print x[i*cols+j] == 1 ? "#" : " "
    }
    puts
  }

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




