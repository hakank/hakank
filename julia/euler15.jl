#=
  Euler #15 in Julia.

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes
  (without backtracking) to the bottom right corner.

  How many routes are there through a 20×20 grid?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function prodlist(from,to)
    return prod(from:to)
end

# 0.01229874s
function euler15a()
    return prodlist(BigInt(21),40) / prodlist(2,20);
end

run_euler(euler15a)
