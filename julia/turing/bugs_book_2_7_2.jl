#=

  The BUGS Book

  Example 2.7.2 Heart transplant cost-effectiveness (continued)

  Cf ~/jags/bugs_book_2_7_2.R
  """
  model {
     thetaN <- 2 # expected lifetime without transplant
     thetaT ~ dbeta(8,2) # probability of surviving operation
     thetaP ~ dnorm(5,1) # expected survival post-transplant (mean 5, sd 1)
     thetaC ~ dnorm(3000,0.000001)
     # expected cost per year (mean 3000, sd 1000)
     E.c <- (20000 + thetaC*thetaT*thetaP)/1000
     # expected additional cost of transplant
     # in thousands of pounds
     E.e <- thetaT*thetaP - thetaN
     # expected total additional survival
     ICER <- E.c/E.e # incremental cost-effectiveness ratio
     for (i in 1:21) {
       K[i] <- (i-1)*5 # constant!
       INB[i] <- E.e*K[i] - E.c
       Q[i] <- step(INB[i])
     }
  }
  """


  Summary Statistics
  parameters        mean         std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol     Float64     Float64    Float64   Float64      Float64   Float64       Float64 

      thetaT      0.8011      0.1208     0.0012    0.0013   10080.2389    1.0000     2794.6324
      thetaP      5.0099      1.0052     0.0101    0.0107    9915.4268    0.9999     2748.9401
      thetaC   3002.9781    317.8311     3.1783    3.2697    9704.9708    1.0000     2690.5935
         E_c     32.0568      3.3304     0.0333    0.0371   10054.3021    0.9999     2787.4417
         E_e      2.0150      1.0209     0.0102    0.0113   10325.3788    0.9999     2862.5946
        ICER     36.5224   1105.9294    11.0593   11.2014   10007.5129    1.0000     2774.4699


  From The WebPPL model:
  expectation:
  [ 'thetaT', 0.798603971520839 ],
  [ 'thetaP', 5.0233592313165465 ],
  [ 'thetaC', 3002.4139520566914 ],
  [ 'E_c', 32.04848460131377 ],
  [ 'E_e', 2.0127388752617894 ],
  [ 'ICER', 23.878283218113094 ] ]


  Cf ~/webppl/bugs_book_2_7_2.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_2_7_2()
    
    thetaN = 2.0                          #  expected lifetime without transplant
    thetaT ~ Beta(8,2)                    #  probability of surviving operation
    thetaP ~ Normal(5,1)                  #  expected survival post-transplant (mean 5, sd 1)
    thetaC ~ Normal(3000.0,sqrt(100000))  #  expected cost per year (mean 3000, sd 1000)
    E_c    ~  Dirac((20000.0 + thetaC*thetaT*thetaP)/1000.0) #  expected additional cost of transplant
    # in thousands of pounds
    E_e   ~ Dirac(thetaT*thetaP - thetaN) #  expected total additional survival
    ICER  ~ Dirac(E_c/E_e)                #  incremental cost-effectiveness ratio
    
    n = 21
    INB = Vector{Real}(undef, n)
    Q = Vector{Real}(undef, n)         
    for i in 1:n
       K = (i-1)*5 #  constant!
       INB[i] ~ Dirac(E_e*K - E_c)
       Q[i] ~ Dirac(INB[i] > 0 ? 1 : 0)
    end

end

model = bugs_book_2_7_2()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(10_000), 10_000)
chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,5), 10_000)

display(chns)
# display(plot(chns))
