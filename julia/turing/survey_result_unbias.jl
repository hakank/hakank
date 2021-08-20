#=

    This is a port of the R2 model SurveyResultUnbias.cs

    With the datafiles
    - survey_result_unbias_population.csv
    - survey_result_unbias_gender.csv
    - survey_result_unbias_person_gender.csv
    - survey_result_unbias_answer.csv

    Note: there is only 5 observations in the answer file, but reading the files makes it look 
          more general...

    Output of the R2 model:
    ```
    <0>[0] Mean: 0.35699               bias[0]
    <0>[0] Variance: 7.87941e-006

    <0>[1] Mean: 0.573151              bias[1]
    <0>[1] Variance: 4.22538e-005

    <1>[0] Mean: 1121.3                mean[0]
    <1>[0] Variance: 77.7373

    <1>[1] Mean: 1557.82               mean[1]
    <1>[1] Variance: 312.151

    <2>[0] Mean: 720.986               variance[0]
    <2>[0] Variance: 6.41627

    <2>[1] Mean: 664.841               variance[1]
    <2>[1] Variance: 7.56368

    <3>[0] Mean: 1125.37               votes[0]
    <3>[0] Variance: 28.9169

    <3>[1] Mean: 1570.39               votes[1]
    <3>[1] Variance: 53.9271
    ```


    The output of this model is a little different than the R2 output:
    Summary Statistics
    parameters        mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
        Symbol     Float64   Float64    Float64   Float64   Float64   Float64       Float64 

         bias[1]      0.8220    0.0000     0.0000    0.0000    3.2352    1.4114        0.2795
         bias[2]      0.3806    0.0000     0.0000    0.0000    5.6540    1.0970        0.4885
        votes[1]   2582.0475    0.0000     0.0000    0.0000    3.2164    1.4135        0.2779
        votes[2]   1034.6060    0.0000     0.0000    0.0000    5.6039    1.0987        0.4842

    pop_mean, pop_variance for gender 0 and 1:
    2-element Vector{Vector{Float64}}:
    [2582.0475011949065, 1034.6060354928363]
    [4.736500784568854e-6, 2.4354395705793703e-6]

   
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function survey_result_unbias(population, person_gender, answer) 
    pop_len = length(population)
    answer_len = length(answer)

    bias ~ filldist(Beta(1,1),pop_len)

    pop_mean = population .*  bias
    pop_variance = (pop_mean .- pop_mean .* bias)
    votes ~ MvNormal(pop_mean, pop_variance)

    ansBias =  Vector{Real}(undef, answer_len) 
    for i in 1:answer_len 
        ansBias[i] = bias[person_gender[i]+1] # gender is 0 and 1 in the person_gender file. Hence the + 1.
        answer[i] ~ Bernoulli(ansBias[i])
    end

    return [pop_mean, pop_variance]
end 

# 3141 2718
population = parse.(Int64,split(readline("survey_result_unbias_population.csv"),","))
# 0 1
# gender = parse.(Int64,split(readline("survey_result_unbias_gender.csv"),","))
# 0 1 0 0 1
person_gender = parse.(Int64,split(readline("survey_result_unbias_person_gender.csv"),","))
# 1 0 1 1 1
answer = parse.(Int64,split(readline("survey_result_unbias_answer.csv"),","))


model = survey_result_unbias(population, person_gender, answer)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 10_000)


display(chns)

println("pop_mean, pop_variance for gender 0 and 1:")
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
display(mean(genq))
# show_var_dist_pct(genq)

