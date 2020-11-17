#=

  Euler #43 in Julia.

  """
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
  each of the digits 0 to 9 in some order, but it also has a rather interesting
  sub-string divisibility property.

  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we
  note the following:

      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17

  Find the sum of all 0 to 9 pandigital numbers with this property.
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# Using all_permutations:
# 1.95482710s
function euler43a()
    primes = [2,3,5,7,11,13,17];
    perms = all_permutations(Array(0:9))
    sum = 0
    for p in perms
        i = 0;
        found = true
        while i < 7 && found == true
            if (100*p[i+2] + 10*p[i+3] + p[i+4]) % primes[i+1] != 0
                found = false
                break
            end
            i += 1
        end

        if found
            sum += parse(Int,join(p,""))
        end
    end

    return sum;
end

# Using next_permutation
# 0.05306371s
function euler43b()
    primes = [2,3,5,7,11,13,17]
    p = [1,0,2,3,4,5,6,7,8,9]
    sum = 0
    rev = Array(9:-1:0)
     while p != rev
        i = 0
        found = true
        while i < 7 && found == true
            if (100*p[i+2] + 10*p[i+3] + p[i+4]) % primes[i+1] != 0
                found = false
                break
            end
            i += 1
        end

        if found
            sum += parse(Int,join(p,""))
        end
        p = next_permutation(p)
    end

    return sum
end


# run_euler(euler43a)
run_euler(euler43b)
