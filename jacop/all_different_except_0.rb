#!/usr/bin/ruby
#
#
# Global constraint all_different_except_0 in Gecode/R
#
# From Global Constraint Catalog
# http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
# """
# Enforce all variables of the collection VARIABLES to take distinct values, 
# except those variables that are assigned to 0.
# """

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

  # this works
  def global_cardinality(xgcc)
    (self[0].domain.max+1).times{|i|
      xgcc[i].must == self.count(i)
    }
  end

  #
  # The simplest implementation and the fastest
  #
  def all_different_except_0
    (1..self[0].domain.max).each{|i|
      self.count(i).must <= 1
    }
  end



end # end Array

class AllDifferentExcept0

  include Gecode::Mixin



  #
  # global cardinality version
  #
  def all_different_except_0_gcc(x)
    max = x[0].domain.max
    gccx_is_an int_var_array(max+1, 0..max)
    x.global_cardinality(gccx)
    (1..max).each{|i|
      gccx[i].must <= 1
    }
  end

  #
  # Using reification.
  # It works, and are slightly faster than the gcc version
  #
  def all_different_except_0_reif(x)
    n = x.length
    b1_is_an bool_var_matrix(n,n)
    b2_is_an bool_var_matrix(n,n)
    b3_is_an bool_var_matrix(n,n)
    n.times{|i|
      n.times{|j|
        if i != j then 
          x[i].must_not.equal(0, :reify => b1[i,j]) 
          x[i].must_not.equal(0, :reify => b2[i,j]) 
          x[i].must_not.equal(x[j], :reify => b3[i,j])
          (b1[i,j] & b2[i,j]).must.imply(b3[i,j])
        else 
          b1[i,j].must.true
          b2[i,j].must.true
          b3[i,j].must.true
        end
      }
    }
  end

  def initialize

    n = 5
    x_is_an int_var_array(n, 0..15)


    # Some extra constraints for easy testing. 
    x.count(0).must >= 2
    x.must_be.sorted

    # counting the number of zeros
    num_zeros_is_an int_var(0..x[0].domain.max+1)
    num_zeros.must == x.count(0)
    
    x.all_different_except_0

    # The other implementations
    # all_different_except_0_gcc(x)
    
    # This works also.
    # all_different_except_0_reif(x)

    branch_on x, :variable => :largest_degree, :value => :max
    # branch_on num_zeros, :variable => :largest_degree, :value => :max

  end # end initialize


end # end class

alldiff = AllDifferentExcept0.new
num_solutions = 0
alldiff.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts "x: #{s.x.values.join(' ')}"
  puts "num_zeros: #{s.num_zeros.value}"

  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"


