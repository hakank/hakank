#!/usr/bin/ruby
#
# Global constraint global cardinality in Gecode/R
#
# See Global Constraint Catalog:
# http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Array


  #
  # sum an array
  #
  def sum
    inject{ |sum, element| sum + element }
  end

  #
  # global cardinality constraint
  #
  def global_cardinality(xgcc)
    (self[0].domain.max).times{|i|
      xgcc[i].must == self.count(i)
    }
  end

end # end Array



class GlobalCardinality

  include Gecode::Mixin

  def initialize

    n = 10

    x_is_an int_var_array(n, 1..n)
    gcc_is_an int_var_array(n,0..n)

    x.global_cardinality(gcc)

    # Some extra constraints for test
    #x[1].must == 3
    gcc.must_be.sorted
    gcc.sum.must == 3

    branch_on x, :variable => :smallest_size, :value => :min 
    branch_on gcc, :variable => :smallest_size, :value => :min 

  end # end initialize


end # end class

gcc = GlobalCardinality.new
num_solutions = 0
gcc.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts "x: #{s.x.values.join(' ')}"
  puts "gcc: #{s.gcc.values.join(' ')}"

  # s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




