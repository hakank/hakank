#=
  Utils for Julia

  Here are utils for my Julia programming.



  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using Printf

# TODO:
# Add the relevant stuff from euler/Euler.jl


#
# Return a Dict of the elements and their occurences
# in the collection a.
#
function make_hash(a)
    d = Dict()
    for e in collect(a)
        # get!(d,e,0)
        if !haskey(d,e)
            d[e] = 0
        end
        d[e] += 1
    end
    return d
end


#
# All k combinations of elements in x without replacements
# (or 1..n if x in an integer).
# cf odometer / combinations_with_replacement
function combinations(k,x::Int) 
    return combinations(k,1:x)
end

function combinations(k,x)
    if k===0
        return [[]]
    elseif x === [] || length(x) === 0
        return []
    else
        y, ys... = x
        t = map(c->append!([y],c),combinations(k-1,ys))
        return append!(t,combinations(k,ys))
    end
    return nothing
end

# julia> [length(all_combinations(1:n)) for n in 1:10]'
# 1×10 adjoint(::Vector{Int64}) with eltype Int64:
# 1  3  7  15  31  63  127  255  511  1023
# i.e. 2^length(x)-1
function all_combinations(x)
    cs = []
    for k in 1:length(x)
        push!(cs,combinations(k,x)...)
    end
    return cs
end

#=
  This is the odometer function: i.e.
  possible ordered combinations of x with replacements.
  If x in an integer, it will be interpreted as the range 0..n-1.

  Cf combinations(k,x)
=#
function odometer(k,x::Int)
    odometer(k, 1:x)
end

function odometer(k,x)
    if k===0
        return [[]]
    elseif x === [] || length(x) === 0
        return []
    else
        first, rest... = x
        t = map(c->append!([first],c), odometer(k-1,x))
        return append!(t, odometer(k,rest))
    end
    return nothing
end 

function combinations_with_replacement(k,x)
    odometer(k,x)
end

# next permutation of array p
function next_permutation(p)
    perm = p
    n = length(perm)
    k = n - 1
    while perm[k] > perm[k+1] && k >= 0
        k = k - 1
    end
    if k > 0
        j = n
        while perm[k] > perm[j]
            j = j - 1
        end
        perm[k],perm[j] = perm[j], perm[k]
        r = n
        s = k + 1
        while r > s
            perm[r],perm[s] = perm[s],perm[r]
            r = r - 1
            s = s + 1
        end
    end
    return perm
end
