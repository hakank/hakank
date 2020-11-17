#=

  Euler #48 in Julia.

  Problem 48:
  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.

  Find the last ten digits of the series,
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")


# 0.00162826s
function euler48a()
    sum = 0
    t = 10_000_000_000
    for i = 1:1_000
        n = i
        for j in 2:i
            n = (n * i) % t
        end
        sum = (sum + n) % t
    end

    return sum
end

run_euler(euler48a)
