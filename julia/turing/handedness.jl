#=
   Handedness 

   From infer.net test/Tests/BlogTests.cs
   """
   probRightHandedActual: Beta(7.72,3.08)[mean=0.7148]
   """

   The Infer.NET model yield beta1=7.72, beta2=3.08 but here
   (and in the WebPPL model) we get different beta parameters:
    beta1=0.62 and beta2=0.56. The prob and probExpected are similar though: 0.6863 vs 0.7148

   For studentData:
   Summary Statistics
     parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
         Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

          betaA    0.6186    0.2511     0.0025    0.0140   289.4811    0.9999      320.5771
          betaB    0.5572    0.2490     0.0025    0.0120   347.0805    1.0014      384.3638
           prob    0.6863    0.1297     0.0013    0.0054   502.4199    1.0000      556.3897
   probExpected    0.7119    0.1406     0.0014    0.0077   302.8398    1.0003      335.3708
  betaAexpected    7.7200    0.0000     0.0000    0.0000    20.5530    0.9999       22.7608
  betaBexpected    3.0800    0.0000     0.0000    0.0000    20.5530    0.9999       22.7608

   For lecturerData:
Summary Statistics
     parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
         Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

          betaA    0.6159    0.2551     0.0026    0.0076   1075.1892    1.0003     1161.1114
          betaB    0.4863    0.2587     0.0026    0.0077   1043.0370    0.9999     1126.3898
           prob    0.8633    0.0980     0.0010    0.0028   1209.6250    1.0000     1306.2905
   probExpected    0.7195    0.1299     0.0013    0.0043    856.3996    0.9999      924.8375
  betaAexpected    7.7200    0.0000     0.0000    0.0000     20.5530    0.9999       22.1954
  betaBexpected    3.0800    0.0000     0.0000    0.0000     20.5530    0.9999       22.1954

  We see that students are slightly less proned to righthandedness than lecturers



   Cf ~/webppl/handedness.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function handedness(data)
    n = length(data)
    
    betaA ~ Uniform(0,1)
    betaB ~ Uniform(0,1)    
    prob ~ Beta(betaA, betaB)
    

    data ~ filldist(flip(prob),n)

    # Value from the Infer.NET model
    probExpected ~ Beta(7.72, 3.08)
    betaAexpected ~ Dirac(7.72)
    betaBexpected ~ Dirac(3.08)    
    
end

# true: is righthanded
studentData = [false, true, true, true, true, true, true, true, false, false];
lecturerData = [false, true, true, true, true, true, true, true, true, true];
# model = handedness(studentData)
model = handedness(lecturerData)

num_chains = 4

# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)
# chns = sample(model, NUTS(), 10_000)

display(chns)
# display(plot(chns))

# show_var_dist_pct(chns, :prob)
# show_var_dist_pct(chns, :probExpected)

