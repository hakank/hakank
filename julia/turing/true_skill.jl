#=
  (Simple) True skill problem.

  Example inspired by
  Johannes Borgstrom, Andrew D. Gordon, Michael Greenberg, James Margetson, and Jurgen Van Gael:
  "Measure Transformer Semantics for Bayesian Machine Learning"
  https://www.microsoft.com/en-us/research/publication/measure-transformer-semantics-for-bayesian-machine-learning-2011/?from=http%3A%2F%2Fresearch.microsoft.com%2Fpubs%2F135344%2Fmsr-tr-2011-18.pdf

  Note that we constraint the generated worlds so they satisfies the
  observation constraints (a > b > c).


  For num_people = 5:

Summary Statistics
      parameters       mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
          Symbol    Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        skill[1]   102.2371    2.8288     0.0283    0.1671   231.3914    0.9999        5.5389
        skill[2]   100.8802    2.5754     0.0258    0.1498   281.8016    1.0053        6.7455
        skill[3]    99.7618    2.5047     0.0250    0.1523   215.7630    1.0022        5.1648
        skill[4]    99.1769    2.7811     0.0278    0.1690   206.5924    1.0058        4.9452
        skill[5]    97.8249    2.8215     0.0282    0.1792   185.3145    1.0026        4.4359
  performance[1]   105.4878    3.5954     0.0360    0.2291   198.0813    1.0005        4.7415
  performance[2]   102.3919    2.5597     0.0256    0.1493   184.1535    1.0264        4.4081
  performance[3]   100.0782    2.5635     0.0256    0.1485   201.1306    1.0179        4.8145
  performance[4]    97.8592    2.7721     0.0277    0.1637   196.4140    1.0023        4.7016
  performance[5]    94.4979    3.2780     0.0328    0.2024   158.5955    1.0044        3.7963

skill[1] > skill[2]: 0.6344
skill[1] > skill[3]: 0.7565
skill[1] > skill[4]: 0.7846
skill[1] > skill[5]: 0.8963
skill[2] > skill[3]: 0.6219
skill[2] > skill[4]: 0.7113
skill[2] > skill[5]: 0.7706
skill[3] > skill[4]: 0.5337
skill[3] > skill[5]: 0.6878
skill[4] > skill[5]: 0.6122
performance[1] > performance[2]:1.0
performance[1] > performance[3]:1.0
performance[1] > performance[4]:1.0
performance[1] > performance[5]:1.0
performance[2] > performance[3]:0.9999
performance[2] > performance[4]:0.9999
performance[2] > performance[5]:0.9995
performance[3] > performance[4]:0.9995
performance[3] > performance[5]:0.9992
performance[4] > performance[5]:0.9992
 

  Cf: ~/cplint/trueskill.pl
      ~/blog/true_skill.blog
      ~/psi/true_skill.psi
      ~/webppl/true_skill.wpl

   Compare with tug_of_war.jl

=#


using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function true_skill(num_people)
    # Each person has an unknown (latent) Skill and a
    # known performance, where the skill is
    # reflected in the performance (with uncertainties).
    skill       = tzeros(num_people)
    performance = tzeros(num_people)
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

# num_people = 3
num_people = 5
model = true_skill(num_people)

num_chains = 4

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(15), 10_000)
# chains = sample(model, SMC(), 10_000)
# chains = sample(model, IS(), 10_000)

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
