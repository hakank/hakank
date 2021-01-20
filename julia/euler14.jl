#=

  Euler #14 in Julia.

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following
  sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1)
  contains
  10 terms. Although it has not been proved yet (Collatz Problem), it is
  thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using Memoization
include("Euler.jl")


# It's much slower with memoizing!
@memoize function hailstone_memo(n)
    if n % 2 == 0
        return round(Int,n / 2)
    else
        return 3*n+1;
    end
end

function hailstone(n)
    if n % 2 == 0
        return round(Int,n / 2)
        # return n // 2
    else
        return 3*n+1;
    end
end

#
# Using 1..1_000_000: 0.68331034s
# Using 3..2..1_000_000: 0.36079831s
function euler14a()
    maxN = 0
    maxLen = 0
    for n in 1:1_000_000
    # for n in 3:2:1_000_000
        m = n
        alen = 0
        while m > 1
            m = hailstone(m)
            alen += 1
        end
        alen += 1
        if alen > maxLen
            maxN = n
            maxLen = alen
        end
    end
    return maxN
end


# Cache the lengths.
# 1..999_999: 1.66648847s
# 3..2..999_999: 1.09878097s
function euler14b()
    hash = Dict()
    maxN = 0
    maxLen = 0
    for n in 2:1_000_000
    # for n in 3:2:1_000_000
        m = n
        mlen = 1
        while m > 1
            if haskey(hash,m)
                mlen = hash[m]+mlen-1
                m = 1
            else
                m = hailstone(m)
                mlen += 1
            end
        end
        if !haskey(hash,n)
            hash[n] = mlen
        end
        if mlen > maxLen
            maxN = n
            maxLen = mlen
        end
    end
    return maxN;
end

# Using an array instead:
# 0.04143852s
function euler14c()
    limit = 1_000_000
    hash = zeros(Int64,limit)
    maxN = 0
    maxLen = 0
    for n = 2:1_000_000
        m = n
        mlen = 1
        while m > 1
            if m <= limit && hash[m] > 0
                mlen = hash[m]+mlen-1
                m = 1
            else
                m = hailstone(m)
                mlen += 1
            end
        end
        if hash[n] == 0
            hash[n] = mlen
        end
        if mlen > maxLen
            maxN = n
            maxLen = mlen
        end
    end
    return maxN;
end


# run_euler(euler14a)
# run_euler(euler14b)
run_euler(euler14c)
