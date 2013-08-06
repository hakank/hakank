#
# Least diff in Gecode/R
#
# Solves the least diff problem: minimizing the diff of ABCDE - FGHIJ.
# 
#

require "rubygems"
require 'gecoder'

solution = Gecode.minimize :diff do

  # A helper to make the linear equation a bit tidier. Takes a number of
  # variables and computes the linear combination as if the variable
  # were digits in a base 10 number. E.g. x,y,z becomes
  # 100*x + 10*y + z .
  def equation_row(*variables)
    variables.inject{ |result, variable| variable + result*10 }
  end

  letters_is_an int_var_array(10, 0..9)
  a,b,c,d,e,f,g,h,i,j = letters
  
  abcde_is_an int_var
  fghij_is_an int_var
  
  equation_row(a,b,c,d,e).must == abcde
  equation_row(f,g,h,i,j).must == fghij

  diff_is_an int_var
  
  # diff.must <= 99999
  diff.must >= 0
  
  (abcde - fghij).must == diff

  a.must_not == 0
  f.must_not == 0
  a.must > f
  
  letters.must_be.distinct
  
  branch_on letters, :variable => :smallest_size, :value => :min

end


puts "diff: #{solution.diff.value}"
puts "a b c d e f g h i j"
puts solution.letters.values.join(' ')

puts "#{solution.abcde.value} - #{solution.fghij.value} = #{solution.diff.value}"
# :propagations => @gecoder_mixin_statistics.propagate,
# :failures     => @gecoder_mixin_statistics.fail,
# :clones       => @gecoder_mixin_statistics.clone,
# :commits      => @gecoder_mixin_statistics.commit,
# :memory       => @gecoder_mixin_statistics.memory

solution.search_stats.each{|stat| puts stat}
