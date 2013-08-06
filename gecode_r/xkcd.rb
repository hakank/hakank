#!/usr/bin/ruby
#
# xkcd knapsackproblem in Gecode/R
#
#  http://xkcd.com/287/
#
# Some amount (or none) of each dish should be order to give a total 
# of exact 15.05.
#

#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#

require 'rubygems'
require 'gecoder'
STDOUT.sync = true

class Array
  # Sums all the elements in the array using #+ .
  def sum
    inject{ |sum, element| sum + element }
  end

  # cross product:
  # i.e. sum(i in 0..n-1) (x[i]*y[i])
  def sum2(y)
    zip(y).map{|a,b| a*b}.inject{|sum,c| sum+c}
  end

end


class XKCD

  include Gecode::Mixin

  def initialize

    # multiplied with 100
    price = [215, 275, 335, 355, 420, 580];

    # how many of each dish?
    x_is_an int_var_array(price.length, 0..1000)

    # number of dishes
    num_dishes_is_an int_var
    x.sum.must == num_dishes

    #
    # total
    #
    total_is_an int_var(0..10000)

    x.sum2(price).must == total

    total.must == 1505; # 15.05 multipled by 100 for using integers

    branch_on x, :variable => :smallest_size, :value => :min
    branch_on num_dishes, :variable => :smallest_size, :value => :min
    branch_on total, :variable => :smallest_size, :value => :min

  end

end

xkcd = XKCD.new
xkcd.minimize!(:num_dishes)
num_solutions = 0
xkcd.each_solution{ |solution| 
  puts "number of dishes: #{solution.num_dishes.value} amount of dishes: #{solution.x.values.join(" ")}"

  num_solutions += 1

  # sol.search_stats.each{|stat, value| puts "#{stat}: #{value}"}
}

puts "num_solutions: #{num_solutions}\n"




