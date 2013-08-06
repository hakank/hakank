#!/usr/bin/ruby
#
# Quasigroup Completion in Gecode/R
# 
# See 
# Carla P. Gomes and David Shmoys:
# "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
#
#
# See also
#
# * Ivars Peterson "Completing Latin Squares"
# http://www.maa.org/mathland/mathtrek_5_8_00.html
# """
# Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
# a four-by-four array so that no column or row contains the same two numbers. 
# The result is known as a Latin square.
#
# ...
#
# The so-called quasigroup completion problem concerns a table that is correctly 
# but only partially filled in. The question is whether the remaining blanks in 
# the table can be filled in to obtain a complete Latin square (or a proper 
# quasigroup multiplication table).
# """
#
# * Global Constraint Catalog, global constraint k_alldifferent
#   http://www.emn.fr/x-info/sdemasse/gccat/Ck_alldifferent.html
#

#
# Compare with the following models:
# * MiniZinc: http://www.hakank.org/minizinc/quasigroup_completion.mzn
# * JaCoP: http://www.hakank.org/JaCoP/QuasigroupCompletion.java
# * Choco: http://www.hakank.org/choco/QuasigroupCompletion.java
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
require 'matrix'
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
  # global constraint k_alldifferent
  # See Global Constraint Catalog
  # http://www.emn.fr/x-info/sdemasse/gccat/Ck_alldifferent.html
  #
  def k_alldifferent
    (self.row_size).times{|i| 
      self.row(i).must_be.distinct(:strength => :domain)
      self.column(i).must_be.distinct(:strength => :domain)
    }
  end

end

class QuasigroupCompletion

  include Gecode::Mixin

  def initialize(m)

    m = Matrix[*m]

    n = m.row_size
    x_is_an int_var_matrix(n, n, 1..n)

    # Data from the input matrix.
    # Values in m with values > 0 are copied to x,
    # values with 0 are unknown and will be calculated.
    #
    n.times{|i|
      n.times{|j|
        x[i,j].must.equal(m[i,j], :strength => :domain) if m[i,j] > 0
      }
    }

    #
    # all rows and columns must be different (k_alldifferent)
    #
    x.k_alldifferent

    branch_on x, :variable => :smallest_size, :value => :min 

  end # end initialize


end # end class


#
# Default problem from 
# Ruben Martins and Inès Lynce
# "Breaking Local Symmetries in Quasigroup Completion Problems", page 3
# The solution is unique.
#    1 3 2 5 4
#    2 5 4 1 3
#    4 1 3 2 5
#    5 4 1 3 2
#    3 2 5 4 1

m = [
     [1, 0, 0, 0, 4],
     [0, 5, 0, 0, 0],
     [4, 0, 0, 2, 0],
     [0, 4, 0, 0, 0],
     [0, 0, 5, 0, 1]
    ]


file = (ARGV[0] || "")
n_sol = (ARGV[1] || 0).to_i

if file.length > 0 and File::exist?(file) then
  t_m = []
  line_no = 0
  t_n = 0
  open(file).each{|line|
    next if line =~ /^(%|#)/ # comment
    next if line =~ /^\s*$/  # empty lines
    line.chomp!
    line.sub!(/^\s+/,"")
    # The first (uncomment) line in the data file contains the dimension,
    # but we don't need this in this model.
    if line_no == 0 then
      line_no += 1
      next
    end
    line_no += 1
    row = line.split(/\s+/).map{|e| e == '.' ? 0 : e; e = e.to_i}
    t_m << row.to_a

  }

  m = t_m

end


quasigroup_completion = QuasigroupCompletion.new(m)
num_solutions = 0
quasigroup_completion.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  x = s.x.values
  x.print_matrix

  #s.search_stats.each{|w,v| puts "#{w}: #{v}"}

  break if n_sol > 0 and num_solutions >= n_sol

}

puts "\nNumber of solutions: #{num_solutions}"
