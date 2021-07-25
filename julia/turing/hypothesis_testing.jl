#=
  From the AgenaRisk model Tutorial/Hypothesis Testing
  Comparison of two materials A and B which has different number of tests of faultyness:

  - A was tested in 200 cases where 10 was faulty
  - B was tested in 100 cases where 9 was fault.

  Is A better then B?

  (Note: This is - yet another variant of A/B tests.)

  Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

  prob_a_is_faulty    0.0548    0.0156     0.0002    0.0013    55.1602    1.0480        7.1572
  prob_b_is_faulty    0.0969    0.0282     0.0003    0.0019   143.2899    1.0096       18.5922
      aBetterThanB    0.9136    0.2810     0.0028    0.0231    60.4603    1.0264        7.8449

  Compare with [10/200, 9/100]: [0.05, 0.09]

  Cf ~/blog/hypothesis_test.blog
     ~/webppl/gaussian_mixture_model2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function hypothesis_testing(a_tests=200,a_faults=10,b_tests=100,b_faults=9)

    prob_a_is_faulty ~ Uniform(0,1)
    prob_b_is_faulty ~ Uniform(0,1)
    
    a_faults ~ Binomial(a_tests,prob_a_is_faulty)
    b_faults ~ Binomial(b_tests,prob_b_is_faulty)

    # Does A has less faults than B?
    aBetterThanB ~ Dirac(prob_a_is_faulty < prob_b_is_faulty)

end

a_tests  = 200
a_faults =  10
b_tests  = 100
b_faults =   9
model = hypothesis_testing(a_tests,a_faults,b_tests,b_faults)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

