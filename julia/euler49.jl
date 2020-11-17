#=

  Euler #49 in Julia.

  Problem 49:
  """
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
  increases by 3330, is unusual in two ways: (i) each of the three terms are
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms
  in this sequence?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function check_perms(n, diff)
    allperms = all_permutations2(digits(n))
    if length(allperms) > 0
        p1 = get_element(n, allperms, diff)
        if p1 != nothing
            p2 = get_element(p1, allperms, diff)
            if p2 != nothing
                return [n, p1, p2]
            end
        end
    end

    return nothing
end

function get_element(n, ll, diff)
    for p in ll
        pp = parse(Int,join(p.|>i->string(i),""))
        if isPrime(pp) && pp > n && pp-n == diff
            return pp
        end
    end
    return nothing
end

# 0.15027299s
function euler49a()
    diff = 3330
    res = 0
    for n in 1001:2:9999
        if n !== 1487 && isPrime(n)
            c = check_perms(n, diff)
            if c != nothing
                res = c
                break
            end
        end
    end

    return parse(Int,join(res,""))

end


run_euler(euler49a)
