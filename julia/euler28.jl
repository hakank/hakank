#=

  Euler #28 in Julia.

  Problem 28:
  """
  Starting with the number 1 and moving to the right in a clockwise
  direction a 5 by 5 spiral is formed as follows:

     21 22 23 24 25
     20  7  8  9 10
     19  6  1  2 11
     18  5  4  3 12
     17 16 15 14 13

  It can be verified that the sum of the numbers on the diagonals is 101.

  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

#
function euler28a()
    s = 1
    n = 3
    while n <= 1001
        s += 4*n^2 - 6*n+6
        n+=2
    end
    return s
end

run_euler(euler28a)
