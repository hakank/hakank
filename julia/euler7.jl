#=

  Euler #7 in Julia.

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00416286s
function euler7a()
    i = 11
    c = 4
    while c < 10001
        if isPrime(i)
            c += 1
        end
        i += 2
    end
    return i-2
end

# 0.00981122s
function euler7b()
    p = primes(200000) # slightly cheating...
    return p[10001];
end


# run_euler(euler7a)
run_euler(euler7b)
