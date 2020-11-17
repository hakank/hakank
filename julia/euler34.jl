#=

  Euler #34 in Julia.

  Problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

  Find the sum of all numbers which are equal to the sum of the
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function factorial_sum(n)
    # return (split(string(n),"").|>i->factorial(parse(Int,i)))|>sum
    return (digits(n).|>i->factorial(i))|>sum

end

# 0.01974533s
function euler34a()
    s = 0;
    for n in 10:100000
        if n == factorial_sum(n)
            s += n
        end
    end
    return s
end

# 0.02070016s
function euler34b()
    return filter(n->n == factorial_sum(n),10:100000)|>sum
end

# 0.01983716s
function euler34c()
    return [n for n in 10:100000 if n == factorial_sum(n)]|>sum
end


# run_euler(euler34a)
# run_euler(euler34b)
run_euler(euler34c)
