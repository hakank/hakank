#=

  Euler #5 in Julia.

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers
  from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers
  from 1 to 20?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00000113s
function euler5a()
    a = 1
    for i in 2:20
        a = lcm(a,i)
    end
    return a
end

#
# 0.00000113s
function euler5b()
    return reduce(lcm,2:20)
end

#
function euler5c()
    return lcm(2:20)
end


run_euler(euler5a)
# run_euler(euler5b)
# run_euler(euler5c)
