#=

  Euler #10 in Julia.

  Problem 10
  """
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/Julia/

=#

include("Euler.jl")

# 0.21296016s
function euler10a()
    return sum(primes(2_000_000))
end

# 0.21336632s
function euler10b()
    p = 2;
    for i in 3:2:2_000_000
        if isPrime(i)
            p+=i
        end
    end
    return p;
end

# 0.04506652s
function euler10c()
    return sieve(2_000_000)|>sum
end

# 0.21216517s
function euler10d()
    return primes(2_000_000)|>sum
end


# run_euler(euler10a)
# run_euler(euler10b)
run_euler(euler10c)
# run_euler(euler10d)
