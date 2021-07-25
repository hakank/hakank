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

  Note: The assumptions are that the slots are filled using a Poisson process
  and the parameter will influence the result much.

  p (mean): 5.7496

  Distributions of variable p (num:0)
  7.00000 =>    3172  (0.317200)
  6.00000 =>    2920  (0.292000)
  5.00000 =>    2349  (0.234900)
  4.00000 =>    1317  (0.131700)
  3.00000 =>     242  (0.024200)

  Distributions of variable total_filled (num:0)
  50.00000 =>   10000  (1.000000)

  Distributions of variable theoretical (num:0)
  224.96027 =>   10000  (1.000000)

  mean sumx: 288.7327



  See ~/blog/coupon_collector.blog
      ~/webppl/cookie_problem.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function coupon_collector()
    n = 50
    m = round(Int,sqrt(n))
    #  "Exact" probability from https:# en.wikipedia.org/wiki/Coupon_collector%27s_problem (footnote [b])
    theoretical ~ Dirac(n*sum([1/i for i in 1:n])) #  N*Math.log(N,Math.e);
    
    #  p = beta(14,14);
    p ~ DiscreteUniform(1,m)

    #  Fill this slot with cards
    fill ~ filldist(Poisson(p),n)

    #  How many slots are filled (i.e. > 0)?
    total_filled ~ Dirac(sum([fill[c] > 0 for c in 1:n]))

    true ~ Dirac(total_filled == n)

    sumx ~ Dirac(sum(fill))
    sum0s ~ Dirac(sum([fill[c] == 0 for c in 1:n] ))
    
end

model = coupon_collector()
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :p)
show_var_dist_pct(chns, :total_filled)
show_var_dist_pct(chns, :theoretical)
println(["mean sumx: ", mean(chns[:sumx].data)])

show_var_dist_pct(chns, Symbol("fill[1]"))
show_var_dist_pct(chns, Symbol("fill[2]"))
show_var_dist_pct(chns, Symbol("fill[3]"))
show_var_dist_pct(chns, Symbol("fill[4]"))
show_var_dist_pct(chns, Symbol("fill[5]"))
show_var_dist_pct(chns, Symbol("fill[6]"))
show_var_dist_pct(chns, Symbol("fill[7]"))
show_var_dist_pct(chns, Symbol("fill[8]"))
show_var_dist_pct(chns, Symbol("fill[9]"))
show_var_dist_pct(chns, Symbol("fill[10]"))


