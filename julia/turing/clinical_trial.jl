#=

    This is a port of the R2 model ClinicalTrial.cs

    with the datafiles 
    - clinical_trial_control.csv  (size 1000)
    - clinical_trial_treatment.csv (size 1000)

    length :1000 mean(control): 0.513 mean(treatment): 0.51

    Output from the R2 model ClinicalTrial.cs (for isEffective)
    ```
    Mean: 0.113
    Variance: 0.100331
    Number of accepted samples = 318
    0:00:14:048.362.900
    ```

    Output from this Turing.jl model:

Summary Statistics
     parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
         Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

    isEffective    0.0659    0.2481     0.0025    0.0140   246.7994    1.0080       25.5512
    probControl    0.5152    0.2910     0.0029    0.0184   182.4067    0.9999       18.8846
  probTreatment    0.5278    0.2789     0.0028    0.0180   173.0546    1.0028       17.9164
        probAll    0.4985    0.0829     0.0008    0.0050   223.0815    1.0112       23.0957

Quantiles
     parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
         Symbol   Float64   Float64   Float64   Float64   Float64 

    isEffective    0.0000    0.0000    0.0000    0.0000    1.0000
    probControl    0.0225    0.2196    0.5287    0.7653    0.9847
  probTreatment    0.0196    0.3207    0.5222    0.8098    0.9720
        probAll    0.1196    0.5019    0.5118    0.5190    0.5348


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function clinical_trial(control,treatment) 
    n = length(control)

    isEffective   ~ flip(0.5)
    probControl   ~ Beta(1,1)
    probTreatment ~ Beta(1,1)
    probAll       ~ Beta(1,1)

    
    if isEffective 
        control   ~ filldist(Bernoulli(probControl),n)
        treatment ~ filldist(Bernoulli(probTreatment),n)
    else
        control   ~ filldist(Bernoulli(probAll),n)
        treatment ~ filldist(Bernoulli(probAll),n)

    end
end 

control = parse.(Int,split(readline("clinical_trial_control.csv"),","))
treatment = parse.(Int,split(readline("clinical_trial_treatment.csv"),","))
println("len(control):$(length(control)) len(treatment):$(length(treatment)) mean(control):$(mean(control)) mean(treatment): $(mean(treatment))")
# println("treatment:$treatment")

model = clinical_trial(control, treatment)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns[[:isEffective,:probControl,:probTreatment,:probAll]])

