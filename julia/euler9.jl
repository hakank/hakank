#=

  Euler #9 in Julia.

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/Julia/

=#

include("Euler.jl")

# 0.00683682s
function euler9a()
    for c in 1:500
        for b in 1:c
            for a in 1:b
                if a+b+c === 1000 && a^2 + b^2 - c^2 == 0
                    return a*b*c;
                end
            end
        end
    end
end

run_euler(euler9a)
