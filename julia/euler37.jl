#=

  Euler #37 in Julia.

  Problem 37:
  """
  The number 3797 has an interesting property. Being prime itself, it is possible to
  continuously remove digits from left to right, and remain prime at each stage:
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right
  and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# check is n is a truncated prime
function truncated_prime(n)
    len = nlen(n)
    for i in 1:len-1
        ii = 10^i
        if !isPrime(n % ii) || !isPrime(div(n,ii))
            return false
        end
    end
    return true
end

# 0.02413909s
function euler37a()
    p = 11
    sum = 0
    c = 0
    while c < 11
        if truncated_prime(p) && isPrime(p)
            c += 1
            sum += p
        end
        p += 2
    end
    return sum
end

run_euler(euler37a)
