#=

    This is a port of the R2 model SurveyResultUnbias2.cs

    With the datafiles
    - survey_result_unbias2_population.csv
    - survey_result_unbias2_gender.csv
    - survey_result_unbias2_person_gender.csv
    - survey_result_unbias2_answer.csv

    Note: there is only 5 observations in the answer file, but reading the files makes it look 
          more general...

    Result from the R2 model:
    ```
    <0>[0] Mean: 0.590981              bias[0]
    <0>[0] Variance: 0.0427666

    <0>[1] Mean: 0.472561              bias[1]
    <0>[1] Variance: 0.0494528

    <1>[0] Mean: 591.075               votes[0]
    <1>[0] Variance: 43109

    <1>[1] Mean: 944.156               votes[1]
    <1>[1] Variance: 197880
    ```

    This model:

Summary Statistics
  parameters        mean        std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol     Float64    Float64    Float64   Float64     Float64   Float64       Float64 

     bias[1]      0.5994     0.2008     0.0020    0.0025   4269.9355    0.9999      385.8259
     bias[2]      0.5022     0.2250     0.0023    0.0035   3757.7959    1.0002      339.5496
    votes[1]    599.4513   201.1412     2.0114    2.5556   4243.2179    0.9999      383.4118
    votes[2]   1004.9705   450.3128     4.5031    6.9752   3788.1458    1.0002      342.2920

Quantiles
  parameters       2.5%      25.0%       50.0%       75.0%       97.5% 
      Symbol    Float64    Float64     Float64     Float64     Float64 

     bias[1]     0.1927     0.4562      0.6150      0.7582      0.9293
     bias[2]     0.1006     0.3207      0.5015      0.6832      0.9061
    votes[1]   191.9750   456.0000    614.0000    754.2500    931.0000
    votes[2]   201.0000   643.0000   1002.0000   1369.0000   1812.0000

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function survey_result_unbias2(population, person_gender, answer) 
    pop_len = length(population)
    answer_len = length(answer)

    bias ~ filldist(Beta(1,1),pop_len)
    votes = tzeros(pop_len)
    for i in 1:pop_len
        votes[i] ~ Binomial(population[i],bias[i])
    end

    ansBias =  Vector{Real}(undef, answer_len) # tzeros(person_gender_len)
    for i in 1:answer_len
        ansBias[i] = bias[person_gender[i]+1]
        answer[i] ~ Bernoulli(ansBias[i])
    end

end 

# 1000 2000
population = parse.(Int64,split(readline("survey_result_unbias2_population.csv"),","))
# 0 1
# gender = parse.(Int64,split(readline("survey_result_unbias2_gender.csv"),","))
# 0 1 0 1 0
person_gender = parse.(Int64,split(readline("survey_result_unbias2_person_gender.csv"),","))
# 1 0 1 1 0
answer = parse.(Int64,split(readline("survey_result_unbias2_answer.csv"),","))


model = survey_result_unbias2(population, person_gender, answer)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)

display(chns)
