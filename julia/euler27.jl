#=

  Euler #27 in Julia.

  Problem 27.
  """
  Euler published the remarkable quadratic formula:

  n^2 + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which
  produces 80 primes for the consecutive values n = 0 to 79. The product of the
  coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic
  expression that produces the maximum number of primes for consecutive
  values of n, starting with n = 0.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")


# 0.04097529s
# with isPrimeCached: 0.07010393s (slower)
function euler27a()
    t = 999
    bestLen = 0
    bestA = 0
    bestB = 0
    for a in -t:t
        for b in -t:t
            len = 0;
            pp = len^2 + a*len + b
            # while pp > 1 && isPrimeCached(pp) # slower
            while pp > 1 && isPrime(pp)
                len += 1
                pp = len^2 + a*len + b;
            end
            if len > bestLen
                bestLen = len
                bestA = a
                bestB = b
            end
        end
    end

    return(bestA * bestB)
end


run_euler(euler27a)
