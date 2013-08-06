#!/usr/bin/ruby
#
# SEND + MORE = MONEY (any base) in Gecode/R
#
# Compare with my MiniZinc model http://www.hakank.org/minizinc/send_more_money_any_base.mzn
#
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#

#
# The number of solutions from base 10 and onward are the triangle numbers, i.e.
#    1,3,6,10,15,21,...
# 
#  Base 10: 1
#  Base 11: 3
#  Base 12: 6
#  Base 13: 10
#  Base 14: 15
#  Base 15: 21
#  etc
# 
# For more about triangle numbers, see http://en.wikipedia.org/wiki/Triangular_number
# 
#
# There seems to be a pattern of the solutions given a base b:
# 
#           S  E  N  D  M  O  R  Y
#           ----------------------
# Base 10:  9  5  6  7  1  0  8  2
#
# Base 11: 10  7  8  6  1  0  9  2
#          10  6  7  8  1  0  9  3
#          10  5  6  8  1  0  9  2
# Base 12:
#          11  8  9  7  1  0 10  3
#          11  8  9  6  1  0 10  2
#          11  7  8  9  1  0 10  4
#          11  6  7  9  1  0 10  3
#          11  6  7  8  1  0 10  2
#          11  5  6  9  1  0 10  2
# 
# ...
# Base 23:
#          22 19 20 18  1  0 21 14
#          22 19 20 17  1  0 21 13
# ...
#
#
# Pattern:
#
# S: always base-1                   e.g. 9 for base 10
# M: always 1                        e.g. 1 any base
# 0: always 0                        e.g. 0 any base
# R: always base-2                   e.g. 8 for base 10
# E, N, D: from base-3 down to 5     e.g. {5,6,7,8,9} for base 10
# Y: between 2 and ???               e.g. {2,3,4} for base 12


require 'rubygems'
require 'gecoder'
STDOUT.sync = true

class Array

  #
  # sum an array in a base
  #
  def toNum(base=10)
    self.inject{ |result, variable| variable + result*base}
  end

end # end Array


class SendMore

  include Gecode::Mixin

  def initialize(base=10)

    letters_is_an int_var_array(8, 0..base-1)
    s,e,n,d,m,o,r,y = letters
    # s,e,n,d,m,o,t,y = letters # SEND + MORE = MONEY

    letters.must_be.distinct
    ([s,e,n,d].toNum(base) + [m,o,r,e].toNum(base)).must == [m,o,n,e,y].toNum(base)
    # ([s,e,n,d].toNum(base) + [m,o,s,t].toNum(base)).must == [m,o,n,e,y].toNum(base)

    s.must_not == 0
    m.must_not == 0

    branch_on letters, :variable => :largest_degree, :value => :max

  end # end initialize


end # end class


def t_num(n) 
   (1..n).to_a.inject(0){|s,e| s+e}
end

base = (ARGV[0] || 10).to_i

#for base in 2..40 do
  t = t_num(base-10+1)
  puts "\nChecking base #{base}"
  send_more = SendMore.new(base)
  num_solutions = 0
  send_more.each_solution{|s| 
    num_solutions += 1
    # puts "\nSolution ##{num_solutions}\n";
    puts s.letters.values.map{|e| "%2d" % e}.join(" ")
    
    # s.search_stats.each{|w,v| puts "#{w}: #{v}"}
    
  }
  
  puts "\nNumber of solutions for base:#{base}: #{num_solutions}  (t_num(#{base}):#{t})"

#end
