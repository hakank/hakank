#!/usr/bin/ruby
#
# send most money, all maximum solutions in Gecode/R
#
# This model simply extends the send_most_money.rb example 
# (http://gecoder.rubyforge.org/examples/send-most-money.html), by first 
# solve the original problem (maximum value of money) and then find 
# all solutions with this value.

#
#
# Model created by Hakan Kjellerstrand, hakank@bonetmail.com
# See also my Gecode/R page: http://www.hakank.org/gecode_r
#


require 'rubygems'
require 'gecoder'

# Solves the cryptarithmetic send+most=money problem while maximizing the value
# of "money".
class SendMostMoney
  include Gecode::Mixin

  attr :money

  # hakank: added parameter
  def initialize(max_money=0)
    # Set up the variables, 9 letters with domain 0..9.
    s,e,n,d,m,o,t,y = letters_is_an int_var_array(8, 0..9)
    # Express the quantity we are optimizing, in this case money.
    # This utilises that any operand can be converted into a variable.
    @money = equation_row(m,o,n,e,y).to_int_var
    
    # Set up the constraints.
    # The equation must hold.
    (equation_row(s, e, n, d) + equation_row(m, o, s, t)).must == 
      equation_row(m, o, n, e, y) 
    
    # The initial letters may not be 0.
    s.must_not == 0
    m.must_not == 0

    # hakank: find all solutions
    if (max_money > 0) then
      money.must == max_money
    end

    # All letters must be assigned different digits.
    letters.must_be.distinct

    # Set the branching.
    branch_on letters, :variable => :smallest_size, :value => :min
  end

  private

  # A helper to make the linear equation a bit tidier. Takes a number of
  # variables and computes the linear combination as if the variable
  # were digits in a base 10 number. E.g. x,y,z becomes
  # 100*x + 10*y + z .
  def equation_row(*variables)
    variables.inject{ |result, variable| variable + result * 10 }
  end
end

puts "\nMaximizing money:"
solution = SendMostMoney.new.maximize! :money
puts 's e n d m o s t y'
puts solution.letters.values.join(' ')
puts "money: #{solution.money.value}"
max_money = solution.money.value

#
# hakank: Now, find all the solutions with this value
#
puts "\nAll solutions with max_money = #{max_money}:"
smm = SendMostMoney.new(max_money)
puts 's e n d m o t y'
num_solutions = 0
smm.each_solution{|s|
  puts "#{s.letters.values.join(' ')}"
  num_solutions += 1
}

puts "\nIt was #{num_solutions} solutions."
