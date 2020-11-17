#=

  Euler #16 in Julia.

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2^1000?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00027433s
function euler16a()
    # return split(string(BigInt(2)^1000),"").|>(i->parse(Int,i))|>sum
    return digits(BigInt(2)^1000)|>sum
end

run_euler(euler16a);
