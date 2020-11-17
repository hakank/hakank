#=
  Euler #24 in Julia.

  Euler 24:
  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are
  listed numerically or alphabetically, we call it lexicographic order. The
  lexicographic permutations of 0, 1 and 2 are:

      012   021   102   120   201   210

  What is the millionth lexicographic permutation of the digits
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """


  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

#
function euler24a()
    a = Array(0:9)
    c = 1
    while c < 1000000
        a = next_permutation(a)
        c += 1
    end

    return parse(Int, join(a,""))
end

# Too slow: 15.24007616s
function euler24b()
    return parse(Int,join(all_permutations2(Array(0:9))[1000000]))
end

# Inspired by a solution on the 'net
# 0.05707951s
function euler24c()
    n = 999999
    p = 10
    eli = (1:p).|>(i->i % 10)
    answer = []
    for i in 1:p-1
        f = factorial(p-i)
        d = floor(Int,n / f)
        n %= f
        push!(answer,eli[d])
        # Note the indexOf()...
        # eli.splice(eli.indexOf(eli[d-1]),1);
        eli = filter(e->e != eli[d], eli)
    end
    return parse(Int,join(vcat(answer,eli)))
end

# all_permutations is much faster than all_permutations2
# 0.49170683s
function euler24d()
    return parse(Int,join(all_permutations(Array(0:9))[1000000]))
end


run_euler(euler24a)
# run_euler(euler24b) # too slow
# run_euler(euler24c)
# run_euler(euler24d)
