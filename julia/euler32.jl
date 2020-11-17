#=

  Euler #32 in Julia.

  Problem 32
  """
  We shall say that an n-digit number is pandigital if it makes use of
  all the digits 1 to n exactly once; for example, the 5-digit number,
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254,
  containing multiplicand, multiplier, and product is 1 through 9
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure
  to only include it once in your sum.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.17556319s
function euler32a()
    prod_hash = Dict{Int,Int}()
    for a in 2:98
        as = string(a)
        if occursin("0",as) continue end
        for b in a+1:9876
            p = a*b
            if occursin("0",string(p)) continue end
            l = "$as$b$p"
            if length(l) == 9 && length(Set(l)) == 9 && !occursin("0",l)
                prod_hash[p] = 1
            end
        end
    end
    return keys(prod_hash)|>sum
end

run_euler(euler32a)
