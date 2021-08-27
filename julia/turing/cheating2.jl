#=
    Cheating model.

    From 
    https://discourse.julialang.org/t/fitting-a-observed-value-to-a-binomial-distribution-turing/66619
    """
    Fitting a observed value to a Binomial Distribution Turing

    I am new to Turing and trying to learn it by trying to replicate the Chapters from the book 
    https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers 2 .

    Here is my problem (from Chapter 2) :

    1.) Have a probability p ~ Uniform(0,1) which I have to infer the “number_of_cheaters”
    2.) p_skewed = p*(0.5) + (0.25) (deterministic value)
    3.) with model:
    yes_responses = pm.Binomial(“number_of_cheaters”, 100, p_skewed, observed=35)

    How do I write this in Turing ?

    ...

    @model function model(yes_responses,N)
        p ~ Uniform(0,1)
        p_skewed = p*(0.5)+0.25
        rv_yes_responses ~ Binomial(N,p_skewed)

        // This line on how to include yes_responses i am unable to figure out

        return p_skewed

    end
   """
    
   The model refered to is in 
   https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter2_MorePyMC/Ch2_MorePyMC_PyMC3.ipynb


    Output of this Turing.jl model:
        Summary Statistics
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

               p    0.2042    0.0993     0.0031    0.0040   446.4728    0.9990      394.4106
        p_skewed    0.3521    0.0497     0.0016    0.0020   446.4728    0.9990      394.4106

    Quantiles
    parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
        Symbol   Float64   Float64   Float64   Float64   Float64 

               p    0.0349    0.1325    0.2020    0.2749    0.3981
        p_skewed    0.2674    0.3162    0.3510    0.3875    0.4490

    Summary Stats:
    Length:         1000
    Missing Count:  0
    Mean:           0.352105
    Minimum:        0.250395
    1st Quartile:   0.316228
    Median:         0.350993
    3rd Quartile:   0.387453
    Maximum:        0.494718


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function cheating_model2(yes_responses,N)
    p ~ Uniform(0,1)
    # Using Dirac(.) makes p_skewed in the chain
    p_skewed ~ Dirac(p*(0.5)+0.25)
    # p_skewed = p*(0.5)+0.25
    yes_responses ~ Binomial(N,p_skewed)

    # An alternative is to return the variable (see below how to handle this)
    return p_skewed
end

yes_response = 35
N = 100
model = cheating_model2(yes_response,N)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  1_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
display(summarystats(vcat(genq...)))
