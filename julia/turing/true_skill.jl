#=
  (Simple) True skill problem.

  Example inspired by
  Johannes Borgstrom, Andrew D. Gordon, Michael Greenberg, James Margetson, and Jurgen Van Gael:
  "Measure Transformer Semantics for Bayesian Machine Learning"
  https://www.microsoft.com/en-us/research/publication/measure-transformer-semantics-for-bayesian-machine-learning-2011/?from=http%3A%2F%2Fresearch.microsoft.com%2Fpubs%2F135344%2Fmsr-tr-2011-18.pdf

  Note that we constraint the generated worlds so they satisfies the
  observation constraints (a > b > c).

  Cf: ~/cplint/trueskill.pl
      ~/blog/true_skill.blog
      ~/psi/true_skill.psi
      ~/webppl/true_skill.wpl

   Compare with tug_of_war.jl

=#

#=
Summary Statistics
      parameters       mean       std   naive_se      mcse        ess      rhat
          Symbol    Float64   Float64    Float64   Float64    Float64   Float64

  performance[1]   104.1935    3.7222     0.0372    0.3157    42.6341    1.0651
  performance[2]   100.4037    2.3667     0.0237    0.1699   105.8531    0.9999
  performance[3]    96.8175    2.8633     0.0286    0.2260    58.5262    1.0032
        skill[1]   101.2267    2.6448     0.0264    0.1878    82.3399    1.0025
        skill[2]   100.5331    2.1493     0.0215    0.1536   124.2788    1.0023
        skill[3]    98.6982    2.5923     0.0259    0.2023    75.9888    1.0281

Quantiles
      parameters      2.5%      25.0%      50.0%      75.0%      97.5%
          Symbol   Float64    Float64    Float64    Float64    Float64

  performance[1]   98.8339   101.0689   103.6526   106.5544   111.5064
  performance[2]   95.9093    98.7352   100.5463   101.6910   104.9909
  performance[3]   92.2724    94.7470    96.3814    99.3117   102.2367
        skill[1]   95.0099    99.4469   101.5057   102.8886   105.4215
        skill[2]   96.3152    99.1755   100.4978   101.9342   104.8146
        skill[3]   93.1567    96.7794    99.0601   100.5969   103.7641

skill[1] > skill[2]
0.606
skill[1] > skill[3]
0.7464
skill[2] > skill[3]
0.6406
performance[1] > performance[2]
0.9997
performance[1] > performance[3]
1.0
performance[2] > performance[3]
1.0
  1.331539 seconds (7.62 M allocations: 386.380 MiB, 8.34% gc time)

=#


using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function true_skill(num_people)
    # There are three people, a, b, and c
    # num_people = 5

    # Each person has an unknown (latent) Skill and a
    # known performance, where the skill is
    # reflected in the performance (with uncertainties).
    # skill = Vector{Float64}(undef, num_people)
    # performance = Vector{Float64}(undef, num_people)
    skill = TArray{Float64}(undef, num_people)
    performance = TArray{Float64}(undef, num_people)
    for p in 1:num_people
        skill[p] ~ Normal(100,sqrt(10))
        performance[p] ~ Normal(skill[p], sqrt(15))
    end

    # Nicer (and slower), but it's not supported by IS or PG!
    # skill .~ Normal(100,sqrt(10))
    # @. performance ~ Normal(skill, sqrt(15)) # Note The @. since we refer to skill[p]

    for p1 in 1:num_people-1, p2 in p1+1:num_people
        true ~ Dirac(performance[p1] > performance[p2])
    end

    # What are their performance and (underlying) skills?

end

num_people = 5
model = true_skill(num_people)
# It seems that MH() is the only sampler that can handle this
# chains = sample(model, MH(), 10000) #
num_chains = 4
chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 1000, num_chains) # PG doesn't support vectorizing assume statement
# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)

# Note: IS don't generate chains the same way as MH, PG, and SMC!
# chains = sample(model, IS(), MCMCThreads(), 1000, num_chains) # IS doesn't support vectorizing assume statement

# NUTS, HMC, and HMCDA throws this error:
# chains = sample(model, NUTS(), 1000) # Error
# chains = sample(model, HMC(0.1, 5), 1000) # Same error as NUTS
# chains = sample(model, HMCDA(0.15, 0.65), 1000) # Same errors as NUTS

display(chains)

# display(plot(chains))
# display(histogram(chains))
# display(gelmandiag(chains)) # requires number of chains >= 2

df = DataFrame(chains);

# Show the latent skills
for p1 in 1:num_people-1, p2 in p1+1:num_people
    println("skill[$p1] > skill[$p2]: ", mean(df[!,"skill[$p1]"] .>df[!,"skill[$p2]"]))
end

# Show the observed performance
for p1 in 1:num_people-1, p2 in p1+1:num_people
    println("performance[$p1] > performance[$p2]:", mean(df[!,"performance[$p1]"] .>df[!,"performance[$p2]"]))
end
