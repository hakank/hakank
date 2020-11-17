#=

  Euler #26 in Julia.

  Problem 26:
  """
  A unit fraction contains 1 in the numerator. The decimal representation of the
  unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
  its decimal fraction part.
  """


  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function get_rep_len(n)
    foundRemainders = zeros(n)
    value = 1;
    position = 1;
    while foundRemainders[value] == 0 && value != 0
        foundRemainders[value] = position
        value = round(Int,1+((value*10) % n)) # added 1
        position += 1
    end
    return position-foundRemainders[value]
end

# 0.00195827s
function euler26a()
    maxLen = 0
    maxD = 0
    for d = 2:1000
        len = get_rep_len(d)
        if len > maxLen
            maxLen = len
            maxD = d
        end
    end
    return maxD
end


# Checks only primes
# 0.00048207s
function euler26b()
    maxLen = 0
    maxD = 0
    for d in 2:1000
        if isPrime(d)
            len = get_rep_len(d)
            if len > maxLen
                maxLen = len
                maxD = d
            end
        end
    end
    return maxD
end

# Only primes
# 0.01460937s
function euler26c()
    return sort(primes(999)
        .|>d->[get_rep_len(d),d]
        , by=x->x[1],rev=true)[1][2]
end

# filter primes first
# 0.01558240s
function euler26d()
    return sort(filter(isPrime, 2:1000)
           .|>d->[get_rep_len(d),d]
           , by=x->x[1],rev=true)[1][2]

end

# A variant. But it's still not complete chaining...
# 0.01890910s
function euler26e()
    return (sort((2:1000).|>x->[get_rep_len(x)*x*isPrime(x),x])|>last)[2]
end


# run_euler(euler26a)
run_euler(euler26b)
# run_euler(euler26c)
# run_euler(euler26d)
# run_euler(euler26e)
