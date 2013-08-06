#!/usr/bin/ruby
#
# Eq20 problem in Gecode/R
#
# Solving 20 linear equations. This is a standard benchmark for solving linear equations. 
#
# Cf Gecode model 
# http://www.gecode.org/gecode-doc-latest/eq20_8cc-source.html
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true

class Eq20

  include Gecode::Mixin

  def initialize

    x_is_an int_var_array(7, 0..10)
    mat = 
    [
     [876370, -16105, 62397, -6704, 43340, 95100, -68610, 58301],
     [533909, 51637, 67761, 95951, 3834, -96722, 59190, 15280],
     [915683, 1671, -34121, 10763, 80609, 42532, 93520, -33488],
     [129768, 71202, -11119, 73017, -38875, -14413, -29234, 72370],
     [752447, 8874, -58412, 73947, 17147, 62335, 16005, 8632],
     [90614, 85268, 54180, -18810, -48219, 6013, 78169, -79785],
     [1198280, -45086, 51830, -4578, 96120, 21231, 97919, 65651],
     [18465, -64919, 80460, 90840, -59624, -75542, 25145, -47935],
     [1503588, -43277, 43525, 92298, 58630, 92590, -9372, -60227],
     [1244857, -16835, 47385, 97715, -12640, 69028, 76212, -81102],
     [1410723, -60301, 31227, 93951, 73889, 81526, -72702, 68026],
     [25334, 94016, -82071, 35961, 66597, -30705, -44404, -38304],
     [277271, -67456, 84750, -51553, 21239, 81675, -99395, -4254],
     [249912, -85698, 29958, 57308, 48789, -78219, 4657, 34539],
     [373854, 85176, -95332, -1268, 57898, 15883, 50547, 83287],
     [740061, -10343, 87758, -11782, 19346, 70072, -36991, 44529],
     [146074, 49149, 52871, -7132, 56728, -33576, -49530, -62089],
     [251591, -60113, 29475, 34421, -76870, 62646, 29278, -15212],
     [22167, 87059, -29101, -5513, -21219, 22128, 7276, 57308],
     [821228, -76706, 98205, 23445, 67921, 24111, -48614, -41906]
    ]

    #
    # note: must encapsule the FixNum with int_var
    #
    20.times{|i|
      (int_var(mat[i][1])*x[0]+
       int_var(mat[i][2])*x[1]+
       int_var(mat[i][3])*x[2]+
       int_var(mat[i][4])*x[3]+
       int_var(mat[i][5])*x[4]+
       int_var(mat[i][6])*x[5]+
       int_var(mat[i][7])*x[6]
       ).must == int_var(mat[i][0])
    }
    
    branch_on x, :variable => :none, :value => :min

  end # end initialize


end # end class


eq20 = Eq20.new
num_solutions = 0
eq20.each_solution{|s| 
  num_solutions +=   1
  puts "\nSolution ##{num_solutions}\n";
  puts "x:#{s.x.values.join(' ')}"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




