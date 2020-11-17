#=
  Euler #20 in Julia.

  Problem 20:
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!")
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00029684s
function euler20a()
    return sum(split(factorial(BigInt(100))|>string,"").|>j->parse(Int,j))
end

# 0.00031793s
function euler20b()
    return sum(digits(factorial(BigInt(100))))
end

# 0.00032192s
function euler20c()
    return factorial(BigInt(100))|>digits|>sum
end

run_euler(euler20a)
# run_euler(euler20b)
# run_euler(euler20c)
