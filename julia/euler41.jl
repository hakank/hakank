#=

  Euler #41 in Julia.

  Problem 41:
  """
  We shall say that an n-digit number is pandigital if it makes use of all
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/Julia_progs/

=#

include("Euler.jl")

# 0.03632750s
function euler41a()
    # Simplification:
    # n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
    #  n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
    n = 7
    m = 0
    while m == 0 && n >= 4
        p = Array(1:n)
        for pp in sort(all_permutations(p),rev=true)
            v = parse(Int,join(pp,""))
            if isPrime(v)
                return v
            end
        end
        n += 1
    end

    return nothing
end

run_euler(euler41a)
