#!/usr/bin/ruby
#
# Diet problem in Gecode/R
#
# Standard Operations Research example in Minizinc
#
#
# Minimize the cost for the products:
# Type of                        Calories   Chocolate    Sugar    Fat
# Food                                      (ounces)     (ounces) (ounces)
# Chocolate Cake (1 slice)       400           3            2      2
# Chocolate ice cream (1 scoop)  200           2            2      4
# Cola (1 bottle)                150           0            4      1
# Pineapple cheesecake (1 piece) 500           0            4      5
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
  # -> sum(i in 0..n-1) (x[i]*y[i])
  def sum2(y)
    zip(y).map{|a,b| a*b}.inject{|sum,c| sum+c}
  end

end


class Diet
  include Gecode::Mixin

  def initialize
    
    n = 4
    price     = [ 50,  20,  30,  80]; # in cents

    limits    = [500,   6,  10,   8]
    calories  = [400, 200, 150, 500]
    chocolate = [  3,   2,   0,   0]
    sugar     = [  2,   2,   4,   4]
    fat       = [  2,   4,   1,   5]

    x_is_an int_var_array(n, 0..1000)

    x.sum2(calories).must  >= limits[0]
    x.sum2(chocolate).must >= limits[1]
    x.sum2(sugar).must     >= limits[2]
    x.sum2(fat).must       >= limits[3]

    cost_is_an int_var
    x.sum2(price).must     == cost
    
    branch_on x, :variable => :smallest_size, :value => :min
    branch_on cost, :variable => :smallest_size, :value => :min

  end
end

diet = Diet.new
diet.minimize!(:cost)
diet.solution{ |solution| 
  # puts solution.x.values.join(',')
  v1,v2,v3,v4 = solution.x.values
  cost = solution.cost.value
  puts "chocolate cake: #{v1} chocolate ice cream: #{v2} cola: #{v3} pineapple cheesecake: #{v4}"
  puts "total cost: #{cost}"
}


