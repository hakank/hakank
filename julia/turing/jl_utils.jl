#=
  Utilities for Turing.jl

  Here are utils for my Turing.jl programming.

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Turing.jl page: http://www.hakank.org/julia/turing/

=#

using Printf, Distributions

"""
Return a Dict of the elements and their occurences
in the collection a.
"""
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

"""
This is to simplify the conversion from WebPPL
"""
function flip(p=0.5)
    return Bernoulli(p)
end


"""
Return a normalized vector, i.e. where the sum is 1
"""
function simplex(v)
    return v./sum(v)
end

"""
Show a sorted Dict with keys, values and percentages
Example
julia> show_var_dist(chains,:d1)

Distributions of variable d1
 1 =>   13654  0.341350
 2 =>   13417  0.335425
 3 =>   12929  0.323225

Note: I'm not sure how to get more than one variables
E.g. this don't work now:
 julia> show_var_dist(chains,[:d1,:d2])
"""
function show_var_dist(chains, var)

    if var in chains.name_map.parameters
        println("Distributions of variable $var")
        len = length(vcat(chains[var]...)) # handle multiple chains
        for kv in sort(make_hash(chains[var]))
            @printf "%-3.5f => % 7d  %2.6f\n" kv[1] kv[2] kv[2]/len
        end
    else
        # println("Variable $var is not in chains")
    end
end

"""
Show distribution of a variable in a MCMCChain
Sort the dictionary in order of decreasing occurrence (percentage)
Examples:

 - show_var_dist_pct(chains, :n)       show all entries

 - show_var_dist_pct(chains, :n, 10)  show first 10 entries (e.g. for large tables)
"""
function show_var_dist_pct(chains::Chains, var, num=0)

    if var in chains.name_map.parameters
        println("Distributions of variable $var (num:$num)")
        len = length(vcat(chains[var]...)) # handle multiple chains
        c = 0
        for kv in sort(collect(make_hash(chains[var])),by=x->x[2],rev=true)
            c += 1
            if (num == 0) || (num > 0 && c <= num)
                @printf "%-3.5f => % 7d  (%2.6f)\n" kv[1] kv[2] kv[2]/len
            end
        end
    else
        # println("Variable $var is not in chains")
    end
end


"""
Show distribution of a variable in a MCMCChain

Sort the dictionary in order of decreasing occurrence (percentage)
and present the values from array `a` where the position in 
`a` represents the values `1..length(a)`.

Examples:

 - show_var_dist_pct(chains, :x, ["yellow","blue", "green"])


"""
function show_var_dist_pct(chains::Chains, var, a::Array)

    if var in chains.name_map.parameters
        println("Distributions of variable $var")
        len = length(vcat(chains[var]...)) # handle multiple chains
        for kv in sort(collect(make_hash(chains[var])),by=x->x[2],rev=true)
            ix = round(Int,kv[1])
            @printf "%-10s => % 7d  (%2.6f)\n" a[ix] kv[2] kv[2]/len
        end
    else
        # println("Variable $var is not in chains")
    end
end


"""
Show distribution of the elements in a (simple) Array.
Sort the dictionary in order of decreasing occurrence (percentage)
Examples:

 - show_var_dist_pct(a)       show all entries

 - show_var_dist_pct(a, 10)  show first 10 entries (e.g. for large tables)

Full example:

julia> show_var_dist_pct(rand(Binomial(100,0.1),100000),10)

 Distributions of variable (num:10)
 10.00000 =>   13143  0.131430
 9.00000 =>   13094  0.130940
 11.00000 =>   12002  0.120020
 8.00000 =>   11403  0.114030
 12.00000 =>    9989  0.099890
 7.00000 =>    8765  0.087650
 13.00000 =>    7475  0.074750
 6.00000 =>    5968  0.059680
 14.00000 =>    5117  0.051170
 5.00000 =>    3450  0.034500
"""
function show_var_dist_pct(a::Array, num=0)
    println("Distributions of variable (num:$num)")
    c = 0
    len = length(a)
    isArray = false
    if length(a[1]) > 1 # typeof(a[1]) <: Array
        isArray = true
    end
    for kv in sort(collect(make_hash(a)),by=x->x[2],rev=true)
        c += 1
        if (num == 0) || (num > 0 && c <= num)
            if isArray
                # This is an array, vector, tuple of elements
                # len = length(kv[1])
                # format = "%-$(len)s => % 7d  %2.6f\n"
                # @printf format kv[1] kv[2] kv[2]/len
                # @printf "%-35s => % 7d  %2.6f\n" kv[1] kv[2] kv[2]/len
                println("$(kv[1])\t=>\t$(kv[2]) ($(kv[2]/len))")
            else
                @printf "%-3.5f => % 7d  (%2.6f)\n" kv[1] kv[2] kv[2]/len
            end
        end
    end
end

"""
This is ported from the WebPPL function in
https://mhtess.github.io/bdappl/chapters/03-simpleModels.html

Example in credible_interval_test.jl:

julia> credible_interval(chains, "posteriorPredictive",0.90)

  credible interval for posteriorPredictive with mass 0.9: (10.000000 .. 18.000000)
"""
function credible_interval(chains::Chains, var, credMass=0.95)
    # Using sort and vcat don't work, and this reshaping is clunky. TODO!
    sss = prod(size(chains[var]))
    chainsCat = reshape(chains[var],sss,1)
    sortedPts = sort(chainsCat,dims=1)
    len = length(sortedPts)
    ciIdxInc = round(Int,ceil(credMass*len))
    nCIs = len - ciIdxInc
    ciWidth = map(i->sortedPts[i+ciIdxInc]-sortedPts[i], 1:nCIs)
    _, i = findmin(ciWidth)
    print("Credible interval for $(var) with mass $(credMass): ")    
    if typeof(sortedPts[1]) == Int64
        @printf("(%d..%d)\n", sortedPts[i],sortedPts[i+ciIdxInc])
    else
        @printf("(%f..%f)\n", sortedPts[i],sortedPts[i+ciIdxInc])
    end

end

"""
Return mean value of the variable var in the chain
"""
function mean_val(chains::Chains, var)
    mean(chains[var])
end

"""
UniformDraw(xs)

Draw one element from `xs` randomly with uniform probability.

For a version with handcrafted probability, see `DiscreteNonParametric`.

"""
UniformDraw(xs) = DiscreteNonParametric(xs, ones(length(xs)) ./ length(xs))

# This works. but one have to call with _varinfo
#  Example
#  # ...
#  observe(_varinfo, d1+d2 == ss)
# Note: We return !cond so it can be called with
#     observe(_varinfo, d1+d2 == ss) && return
# function observe(_varinfo,cond)
#    cond || Turing.@addlogprob! -Inf
#    return !cond
# end

#  This works. but one have to call with _sampler and  _varinfo
#  Example
#  # ...
#  observe(_sampler, _varinfo, d1+d2 == ss)
#
# See Dirac() for a better solution
#
#=
function obs!(_sampler,_varinfo,cond)
    if !cond
        if _sampler isa Turing.Sampler{<:Union{PG,SMC}}
            produce(-Inf)
        else
            Turing.@addlogprob! -Inf
        end
    else
        if _sampler isa Turing.Sampler{<:Union{PG,SMC}}
            produce(0.0)
        end
    end
    return !cond
end
=#

#
# Ensure ("observe") than an condition is met.
# Usage:
#   @model function two_dice(ss)
#    d1 ~ DiscreteUniform(1, 6)
#    d2 ~ DiscreteUniform(1, 6)
#
#    ss ~ Dirac(d1 + d2)
#    true ~ Dirac(d > 2) # note true ~ ...
# and
#
#
# From David Widdman in https://github.com/TuringLang/Turing.jl/issues/1471
#
# Note: This is now integrated in Distributions.jl
# struct Dirac{T} <: ContinuousUnivariateDistribution
#     x::T
# end
# Distributions.logpdf(d::Dirac, x::Real) = x == d.x ? 0.0 : -Inf
