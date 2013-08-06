#!/usr/bin/ruby
#
#
# Map coloring in Gecode/R
#
# Simple coloring using a connection matrix.
#
# Compare with the MiniZinc model http://www.hakank.org/minizinc/map.mzn
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
require 'enumerator'
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

  #
  # assign color to each country
  # neighbour countries must not have the same color
  #
  def coloring(connections)
    @n = connections.length

    for i in 0..@n-1 do
      for j in 0..@n-1 do
        if i != j and connections[i][j] > 0 then
          self[i].must_not == self[j]
        end
      end
    end
    
  end # end coloring
  
  
end # end Array


#
# The problem
#
class MapColoring
  
  include Gecode::Mixin
  
  def initialize
    
    # [belgium, denmark, france, germany, netherlands, luxembourg]
    #
    # connection matrix: 1 if connected, else 0
    #
    connections = [
                   [0, 0, 1, 1, 1, 1],
                   [0, 0, 0, 1, 0, 0],
                   [1, 0, 0, 1, 1, 0],
                   [1, 1, 1, 0, 1, 1],
                   [1, 0, 1, 1, 0, 0],
                   [1, 0, 0, 1, 0, 0]
                  ];

    n = connections.length
    # num_countries_is_an int_var
    # num_countries.must == n

    # the coloring for each country
    colors_is_an int_var_array(n, 0..n-1)

    # color the map
    colors.coloring(connections)

    # symmetry breaking: first country has color 0
    colors[0].must == 0

    # symmetry breaking which may not scale
    # colors.must_be.sorted

 
    # number of times a color is used
    colors_used_is_an int_var_array(n, 0..n)
    n.times{ |i| colors.count(i).must == colors_used[i] }

    # don't work
    # for i in 1..n-1 do
    #   if colors_used[i-1] == int_var(0) then 
    #      colors_used[i].must == int_var(0)
    #  end
    #end

    # number of colors used, which is calculated by checking
    # how many colors that were _not_ used.
    num_used_is_an int_var(0..n)

    # Note: n - colors_used.count(0) is not allowed
    # must convert n to int_var first
    num_used.must == int_var(n) - colors_used.count(0)

    branch_on colors      , :variable => :largest_degree, :value => :max
    branch_on colors_used , :variable => :largest_degree, :value => :max
    branch_on num_used    , :variable => :largest_degree, :value => :max
    
  end # end initialize
  
  
end # end class


countries = ["belgium", "denmark", "france", "germany", "netherlands", "luxembourg"]
map_coloring = MapColoring.new
map_coloring.minimize!(:num_used)
num_solutions = 0
map_coloring.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}";
  puts countries.map{|e| "#{e}->"}.zip(s.colors.values).join(" ")
  puts s.colors_used.values.join(" ")
  puts "colors used: #{s.num_used.value}"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




