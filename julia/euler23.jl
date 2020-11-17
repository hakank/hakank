#=

  Euler #23 in Julia.

  Problem 23:
  """
  A perfect number is a number for which the sum of its proper divisors
  is exactly equal to the number. For example, the sum of the proper divisors
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number
  that can be written as the sum of two abundant numbers is 24. By mathematical
  analysis, it can be shown that all integers greater than 28123 can be written
  as the sum of two abundant numbers. However, this upper limit cannot be reduced
  any further by analysis even though it is known that the greatest number that
  cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of
  two abundant numbers.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

#
# Note: This is a solution ported from my JavaScript program via Picat code
# which in turn is from some C++(?) code,
# 0.00790154s
function euler23a()
    limit = 20161;
    arr = ones(limit)
    for i in 2:limit
        for j in i*2:i:limit
            arr[j] = arr[j] + i
        end
    end

    abundant = [i for i in 12:limit if arr[i] > i]
    for a in abundant
        for b in abundant
            if b > a || a + b >= limit
                break;
            else
                arr[a + b] = 0
            end
        end
    end

    return sum([i for i in 1:limit if arr[i] != 0])
end

run_euler(euler23a)
