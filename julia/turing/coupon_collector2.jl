#=

  Coupon collecter's problem, or card collecter's problem.

  There are N different collecter's cards hidden in a package, but we don't
  know which card there is in the package we buy.
  We want to collect all of them, how many packages must one buy to collect
  all the different cards?

  See https://en.wikipedia.org/wiki/Coupon_collector%27s_problem
  """
  In probability theory, the coupon collector's problem describes 'collect all coupons and win'
  contests. It asks the following question: If each box of a brand of cereals contains a
  coupon, and there are n different types of coupons, what is the probability that more
  than t boxes need to be bought to collect all n coupons?

  An alternative statement is: Given n coupons, how many coupons do you expect you need
  to draw with replacement before having drawn each coupon at least once? The mathematical
  analysis of the problem reveals that the expected number of trials needed grows as
  Θ(n log(n).
  For example, when n = 50 it takes about 225[b] trials on average to collect all 50 coupons.

  ...

  [b]: E(50) = 50(1 + 1/2 + 1/3 + ... + 1/50) = 224.9603, the expected number of trials to
  collect all 50 coupons.

   The approximation n*log(n) + γ*n + 1/2 for this expected number gives in this case
   ≈ 195.6011 + 28.8608 + 0.5 ≈ 224.9619.  [log is the natural logarithm]
  """

  This version draw random integers until all integers 0..N-1 have been selected.
  We only study the length of the generated array.

  NOTE: THIS IS NOT A TURING MODEL.
  Just a plain simulation...

=#

#=
Running n in [10,20,30,50,100,200]
julia> @time include("coupon_collector2.jl")
n:10 theoretical prob:29.289682539682538  s:1000
mean:29.074
Theoretical: 29.289682539682538
  0.029726 seconds (883.17 k allocations: 31.116 MiB)

n:20 theoretical prob:71.95479314287364  s:1000
mean:71.929
Theoretical: 71.95479314287364
  0.241385 seconds (4.69 M allocations: 173.947 MiB, 16.75% gc time)

n:30 theoretical prob:119.84961392761171  s:1000
mean:118.299
Theoretical: 119.84961392761171
  0.499929 seconds (11.57 M allocations: 362.730 MiB, 8.86% gc time)

n:50 theoretical prob:224.96026691647114  s:1000
mean:222.996
Theoretical: 224.96026691647114
  2.083751 seconds (38.59 M allocations: 1.326 GiB, 8.76% gc time)

n:100 theoretical prob:518.737751763962  s:1000
mean:521.378
Theoretical: 518.737751763962
  9.139250 seconds (193.93 M allocations: 5.524 GiB, 8.02% gc time)

n:200 theoretical prob:1175.6061896242888  s:1000
mean:1170.562
Theoretical: 1175.6061896242888
 49.765464 seconds (1.13 G allocations: 32.495 GiB, 7.64% gc time)

 61.848598 seconds (1.38 G allocations: 39.908 GiB, 7.77% gc time)

=#


using Distributions
include("jl_utils.jl")

# Theoretical length of n coupons
# "Exact" probability from https://en.wikipedia.org/wiki/Coupon_collector%27s_problem
# (see footnote [b] above)
function theoretical_prob(n)
    return n*sum([1/i for i in 1:n])
end

function coupon_collect(n,arr)
    if length(arr) > 0 && length(unique(arr))==n
        return arr
    else
        r = rand(1:n) # a little faster
        # return coupon_collect(n,vcat(arr, r))
        return coupon_collect(n,push!(arr, r)) # slightly faster
    end
end

function run_coupon_collect(n=10,s=1000)

    theoretical = theoretical_prob(n)
    println("n:$n theoretical prob:$(theoretical_prob(n))  s:$s")

    lengths = []
    for t in 1:s
        c = coupon_collect(n,[])
        collectLen = length(c)
        # println("c:$c")
        push!(lengths,collectLen)
    end

    println("mean:", mean(lengths))
    println("Theoretical: ", theoretical)
end

n=100
@time run_coupon_collect(n,1000)
#=
for n in [10,20,30,50,100,200]
    @time run_coupon_collect(n,1000)
    println()
end
=#
