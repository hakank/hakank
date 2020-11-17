#=

  Euler #50 in Julia.

  Problem 50:
  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime,
  contains 21 terms, and is equal to 953.

  Which prime, below one-million, can be written as the sum of the most
  consecutive primes?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00700822s
function euler50a()
    n = 10000
    p = primes(n)
    for len in 550:-1:21
        for offset in 1:549
            pp = ((offset+1:offset+len).|>i->p[i])|>sum
            if pp < 1000000 && isPrime(pp)
                return pp
            end
        end
    end

    return nothing
end

run_euler(euler50a)
