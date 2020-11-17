#=

  Euler #38 in Julia.

  """
  Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

  By concatenating each product we get the 1 to 9 pandigital,
  192384576. We will call 192384576 the concatenated product of 192
  and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the
  concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be
  formed as the concatenated product of an integer with
  (1,2, ... , n) where n > 1?
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00016527s
function euler38a()
    for n in 9876:-1:9
        println
        s = string(n)
        i = 2
        while length(s) < 9
            s *= string(n*i)
            i += 1
        end
        if length(s) == 9 && is_pandigital(s)
            return parse(Int,s)
        end
    end
    return nothing
end

run_euler(euler38a)
