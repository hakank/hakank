#=

  Euler #30 in Julia.

  Problem 30
  """
  Surprisingly there are only three numbers that can be written
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of
  fifth powers of their digits.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.07169755ss
function euler30a()
    t = 0
    m = 5
    for n in 10:6 * (9^5)
        # nn = (split(string(n),"").|>i->parse(Int,i)^m)|>sum
        nn = (digits(n).|>i->i^m)|>sum
        if n === nn
            t += n
        end
    end
    return t
end

# 0.15361390s
function euler30b()
    m = 5;
    # s = (10:6*9^5).|>n-> n*(n==(split(string(n),"").|>i->parse(Int,i)^m)|>sum)
    s = (10:6*9^5).|>n-> n*(n==(digits(n).|>i->i^m)|>sum)
    return s|>sum
end

run_euler(euler30a)
# run_euler(euler30b)
