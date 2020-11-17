#=

  Euler #6 in Julia.

  Problem 6
  """
  The sum of the squares of the first ten natural numbers is,
  1^(2) + 2^(2) + ... + 10^(2) = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00000693s
function euler6a()
    s1 = sum(1:100)^2
    s2 = sum((1:100).|>i->i^2)
    return s1-s2;
end

# 0.00000085s
function euler6b()
    sum(1:100)^2 - sum((1:100).|>i->i^2)
end

# run_euler(euler6a)
run_euler(euler6b)
