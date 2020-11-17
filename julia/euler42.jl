#=

  Euler #42 in Julia.

  Problem 42:
  """
  The nth term of the sequence of triangle numbers is given by,
      tn = 1/2*n*(n+1);
  so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its
  alphabetical position and adding these values we form a word value. For example,
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
  is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
  containing nearly two-thousand common English words, how many
  are triangle words?
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# n'th triangle number
function triangle_number(n)
    return floor(Int,(n*(n-1)) / 2)
end

# get the score for a name
function get_score(name)
    return ([name[i] for i in 1:length(name)]
           .|>c->(Int(c)-64))|>sum
end

# 0.09908583s
function euler42a()
    t20 = Set((1:20).|>i->triangle_number(i))
    words = split( (readline("euler42_words.txt").|>word->replace(word,"\""=>"")),",")
    return (words
           .|>word-> get_score(word) in t20
           )|>sum
end

run_euler(euler42a)
