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

   b) Problem 2
      find the probability that at least two of the digits 0..9
      appears exactly once.
      Answer: 1179/1250 ~ 0.9432

=#

using Turing # , StatsPlots, DataFrames
include("jl_utils.jl")

# Note that we collect the interesting data to post process it.
# This is because we cannot add non random values in the chain.
k_occ_coll1 = []
# occ_coll = []
@model function six_digits(n, k, m)
        # d = TArray{Int}(undef, n)
        d = Vector{Int}(undef, n)
        for i in 1:n
            d[i] ~ DiscreteUniform(0,9)
        end

        # Number of occurrences of each digit in digits
        occ = make_hash(d)

        # push!(occ_coll, occ)
        # Number of digits that occurs exactly k times
        k_occ = sum(values(occ).|>x->x==k)>=m

        push!(k_occ_coll1, k_occ)
end

# 1) Find the probability that at least one of the digits 0..9
#    appears exactly twice.
println("1) Find the probability that at least one of the digits 0..9\nappears exactly twice.")
model = six_digits(6,2,1)
num_chains = 4
num_samples = 10_000
chains = sample(model, MH(), MCMCThreads(), 1000, num_chains)

# It seems that MH() is the only sampler that can handle this
# chains = sample(model, MH(), 10000) #
# chains = sample(model, IS(), MCMCThreads(), num_samples, num_chains)
# chains = sample(model, PG(20), MCMCThreads(), 1000, num_chains)
# chains = sample(model, SMC(1000), MCMCThreads(), 1000, num_chains)

# NUTS, HMC, and HMCDA throws this error:
# chains = sample(model, NUTS(), 1000) # Error
# chains = sample(model, HMC(0.1, 5), 1000) # Same error as NUTS
# chains = sample(model, HMCDA(0.15, 0.65), 1000) # Same errors as NUTS

# display(chains)

# display(group(chains,:d))

# println("occ_coll:$occ_coll")
println("mean: $(sum(k_occ_coll1)/length(k_occ_coll1))")
display(sort(make_hash(k_occ_coll1)))


# Note: Since we are updating an external array we have to
# build a new model here...


# 2) Find the probability that at least two of the digits 0..9
#    appears exactly once.
k_occ_coll2 = []
@model six_digits_2(n, k, m) = begin
        # d2 = TArray{Int}(undef, n)
        d2 = Vector{Int}(undef, n)
        for i in 1:n
            d2[i] ~ DiscreteUniform(0,9)
        end

        # Number of occurrences of each digit in digits
        occ2 = make_hash(d2)

        # Number of digits that occurs exactly k times
        k_occ2 = sum(values(occ2).|>x->x==k)>=m
        push!(k_occ_coll2, k_occ2)
end

println("\n2) Find the probability that at least two of the digits 0..9\nappears exactly once.")
model2 = six_digits_2(6,1,2)

num_chains = 4
num_samples = 10_000
chains = sample(model2, MH(), MCMCThreads(), num_samples, num_chains)

println("mean: $(sum(k_occ_coll2)/length(k_occ_coll2))")
display(sort(make_hash(k_occ_coll2)))
