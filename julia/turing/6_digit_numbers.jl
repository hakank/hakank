#=
   6 digit numbers

   From
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 19f, Problem 2.5 Problems concerning random numbers

   Given the 6 digits numbers:

   a) Problem 1
      find the probability that at least one of the digits 0..9
      appears exactly twice.
      Answer: 2943/4000 ~ 0.7358

      Distributions of variable k_occ (num:0)
      1.00000 =>    4921  (0.492100)
      0.00000 =>    2656  (0.265600)
      2.00000 =>    2322  (0.232200)
      3.00000 =>     101  (0.010100)

      Distributions of variable k_occ_m (num:0)
      1.00000 =>    7344  (0.734400)
      0.00000 =>    2656  (0.265600)


   b) Problem 2
      find the probability that at least two of the digits 0..9
      appears exactly once.
      Answer: 1179/1250 ~ 0.9432

      Distributions of variable k_occ (num:0)
      4.00000 =>    4450  (0.445000)
      2.00000 =>    2404  (0.240400)
      6.00000 =>    1488  (0.148800)
      3.00000 =>    1044  (0.104400)
      1.00000 =>     503  (0.050300)
      0.00000 =>     111  (0.011100)

      Distributions of variable k_occ_m (num:0)
      1.00000 =>    9386  (0.938600)
      0.00000 =>     614  (0.061400)

=#

using Turing # , StatsPlots, DataFrames
include("jl_utils.jl")

#
# Find the probability that at least m digits appears exactly k times
#
@model function six_digits(n, k, m)
    d ~ filldist(DiscreteUniform(0,9),n)
    digits = tzeros(10)
    for digit in 0:9
        digits[digit+1] ~ Dirac(sum([d[i] == digit ? 1 : 0 for i in 1:n]))
    end
    # Count of exactly k occurrences
    k_occ ~ Dirac(sum([digits[i] == k ? 1 : 0 for i in 1:10]))
    # Is k_occ >= m
    k_occ_m ~ Dirac(k_occ >= m)    
end

# 1) Find the probability that at least one of the digits 0..9
#    appears exactly twice.
println("1) Find the probability that at least one of the digits 0..9\nappears exactly twice.")
model = six_digits(6,2,1)
num_chains = 4
num_samples = 10_000

chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(20), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), num_samples)

# display(chns)
show_var_dist_pct(chns,:k_occ)
show_var_dist_pct(chns,:k_occ_m)

# 2) Find the probability that at least two of the digits 0..9
#    appears exactly once.
println("\n2) Find the probability that at least two of the digits 0..9\nappears exactly once.")
model = six_digits(6,1,2)

chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(20), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), num_samples)

# display(chns)

show_var_dist_pct(chns,:k_occ)
show_var_dist_pct(chns,:k_occ_m)
