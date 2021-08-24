#=

    This is a port of the SPPL model ClinicalTrial.pynb

    Note: There is no data in this SPPL model, so I take 
    the data from clinical_trial.jl (which is a port of the R2 model ClinicalTrial.cs)

    with the datafiles 
    - clinical_trial_control.csv  (size 1000)
    - clinical_trial_treatment.csv (size 1000)

    The SPPL model takes 20 observations and give this exact probability:
       p(isEffective): 0.3658327735005218


    This model:
    Summary Statistics
        parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        isEffective    0.3603    0.4801     0.0048    0.0116   1739.9975    1.0002      178.8465
        probControl    9.5703    5.1312     0.0513    0.1206   1744.4818    1.0019      179.3074
      probTreatment   10.3307    5.1155     0.0512    0.1228   1789.7505    1.0001      183.9604
            probAll    9.7317    3.9333     0.0393    0.0986   1621.2317    1.0014      166.6391
           pControl    0.4785    0.2566     0.0026    0.0060   1744.4818    1.0019      179.3074
         pTreatment    0.5165    0.2558     0.0026    0.0061   1789.7505    1.0001      183.9604




    Cf clinical_trial.jl (which use Bernoilli on the probabilities instead of DiscreteUniform.)

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function clinical_trial(control,treatment) 
    n = length(control)

    isEffective   ~ flip(0.5)
    probControl   ~ DiscreteUniform(0,n)
    probTreatment ~ DiscreteUniform(0,n)
    probAll       ~ DiscreteUniform(0,n)

    if isEffective 
        control   ~ filldist(Bernoulli(probControl/n),n)
        treatment ~ filldist(Bernoulli(probTreatment/n),n)
    else
        control   ~ filldist(Bernoulli(probAll/n),n)
        treatment ~ filldist(Bernoulli(probAll/n),n)

    end

    # The probabilities
    pControl ~ Dirac(probControl / n)
    pTreatment ~ Dirac(probTreatment / n)
end 

control = parse.(Int,split(readline("clinical_trial_control.csv"),","))
treatment = parse.(Int,split(readline("clinical_trial_treatment.csv"),","))
println("len(control):$(length(control)) len(treatment):$(length(treatment)) mean(control):$(mean(control)) mean(treatment): $(mean(treatment))")
# println("treatment:$treatment")

n = 20
model = clinical_trial(control[1:n], treatment[1:n])

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

# display(chns[[:isEffective,:probControl,:probTreatment,:probAll,:pControl,:pTreatment]])
display(chns)


