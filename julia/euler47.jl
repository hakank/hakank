#=

  Euler #47 in Julia.

  Problem 47:
  """
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 x 7
  15 = 3 x 5

  The first three consecutive numbers to have three distinct
  prime factors are:

  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.

  Find the first four consecutive integers to have four distinct primes
  factors. What is the first of these numbers?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.01653001s
function euler47a()
    maxn = 1_000_000
    f = zeros(maxn)
    for i in 2:maxn-1
        if f[i] == 0
            for j in 2*i:i:maxn-1
                f[j] += 1
            end
        end
    end

    goal = [4,4,4,4]
    found = 0
    for i in 1:maxn-3
        if [f[i],f[i+1],f[i+2],f[i+3]] == goal
            found = i
            break
        end
    end
    return found
end

run_euler(euler47a)
