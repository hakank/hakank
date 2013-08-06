#!/usr/bin/ruby
#
# Seseman problem in Gecode/R
#
# Simple recreational mathematics problem.
#
# Compare with my other models
# - MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
# - JaCoP: http://www.hakank.org/JaCoP/Seseman.java
# - Choco: http://www.hakank.org/choco/Seseman.java
#
# and the CGI program "Seseman's Convent Problem"
# http://www.hakank.org/seseman/seseman.cgi
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#

require 'rubygems'
require 'gecoder'


class Array
  # Sums all the elements in the array using #+ .
  def sum
    inject{ |sum, element| sum + element }
  end

end


class Seseman
  include Gecode::Mixin

  def initialize
    rowsum_is_an int_var
    totalsum_is_an int_var

    #rowsum.must == 9
    totalsum.must == 24

    a,b,c,d,e,f,g,h = vars_is_an int_var_array(8,1..9)

    (a+b+c).must == rowsum
    (a+d+f).must == rowsum
    (c+e+h).must == rowsum
    (f+g+h).must == rowsum

    vars.sum.must == totalsum

    # vars[0].must == 1 # symmetry breaking
   
    branch_on vars, :variable => :smallest_size, :value => :min
    branch_on rowsum, :variable => :smallest_size, :value => :min
    branch_on totalsum, :variable => :smallest_size, :value => :min

  end
end

seseman = Seseman.new
num_solutions = 0
seseman.each_solution{ |solution| 
  (a,b,c,d,e,f,g,h) = solution.vars.values
  rowsum = solution.rowsum.value
  totalsum = solution.totalsum.value
  puts "\nrowsum:#{rowsum} totalsum:#{totalsum}\n#{a} #{b} #{c}\n#{d} _ #{e}\n#{f} #{g} #{h}\n"
  num_solutions += 1
}

puts "num_solutions: #{num_solutions}\n"
