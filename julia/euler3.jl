#=

  Euler #3 in Julia.

  Problem 3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


include("Euler.jl")

# 0.00354527s
function euler3a()
    n = 600851475143
    max = 0
    while (n > 1)
        m = round(Int,ceil(sqrt(n)))
        for i in 2:m
            if (n % i == 0 && isPrime(i))
                max = i;
                n /= i;
                continue;
            end
        end
    end
    return max;

end

# 0.00607204s
function euler3b()
    return maximum(prime_divisors(600851475143))
end

# 0.00169180s
function euler3c()
    return maximum(factors(600851475143))
end


# run_euler(euler3a)
# run_euler(euler3b)
run_euler(euler3c)
