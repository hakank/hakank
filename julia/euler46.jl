#=

  Euler #46 in Julia.

  Problem 46:
  """
  It was proposed by Christian Goldbach that every odd composite number can be
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the
  sum of a prime and twice a square?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00057412s
function euler46a()
    res = 0
    gotit = false
    for i in 3:2:10000
        if !isPrime(i) && !gotit
            s = round(Int,sqrt(i/2))
            found = 0
            for j in 1:s
                if found === 0
                    ts = j*j*2
                    if isPrime(abs(i-ts))
                        found = 1
                    end
                end
            end
            if found == 0
                res = i
                gotit = true
                break
            end
        end
    end
    return res
end

run_euler(euler46a)
