#!/usr/bin/ruby
#
#
# ALPHA problem (alphametic problem) in Gecode/R
#
# This is a standard alphametic problem in mathematical recreations, 
# constraint programming etc.
#

#
# Cf the MiniZinc models http://www.hakank.org/minizinc/crypto.mzn
#                        http://www.hakank.org/minizinc/crypto_ip.mzn
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'
STDOUT.sync = true


class Alpha

  include Gecode::Mixin

  
  def sum(*vars)
    vars.inject{ |res, var| var + res }
  end

  def initialize

    n = 26
    letters_is_an int_var_array(26, 1..26)
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z = letters

    letters.must_be.distinct

    sum(b,a,l,l,e,t).must       == 45
    sum(c,e,l,l,o).must         == 43
    sum(c,o,n,c,e,r,t).must     == 74
    sum(f,l,u,t,e).must         == 30
    sum(f,u,g,u,e).must         == 50
    sum(g,l,e,e).must           == 66
    sum(j,a,z,z).must           == 58
    sum(l,y,r,e).must           == 47
    sum(o,b,o,e).must           == 53
    sum(o,p,e,r,a).must         == 65
    sum(p,o,l,k,a).must         == 59
    sum(q,u,a,r,t,e,t).must     == 50
    sum(s,a,x,o,p,h,o,n,e).must == 134
    sum(s,c,a,l,e).must         == 51
    sum(s,o,l,o).must           == 37
    sum(s,o,n,g).must           == 61
    sum(s,o,p,r,a,n,o).must     == 82
    sum(t,h,e,m,e).must         == 72
    sum(v,i,o,l,i,n).must       == 100
    sum(w,a,l,t,z).must         == 34
    
    branch_on letters, :variable => :smallest_size, :value => :min 


  end # end initialize


end # end class

alpha = Alpha.new
num_solutions = 0
alpha.each_solution{|s| 
  num_solutions += 1
  puts "\nSolution ##{num_solutions}\n";
  puts "letters: #{s.letters.values.join(' ')} "
  puts "letters:"
  puts ('a'..'z').zip(s.letters.values).map{|l,v| "#{l}=#{v}"}.join(' ')
  puts
  s.search_stats.each{|w,v| puts "#{w}: #{v}"}

}

puts "\nNumber of solutions: #{num_solutions}"




