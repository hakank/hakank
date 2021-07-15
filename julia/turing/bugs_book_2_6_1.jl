#=
   The BUGS Book
   Example 2.6.1 Heart transplant cost-effectiveness: risks assumed known

   Cf ~/jags/bugs_book_2_6_1.R
   """
   model {
      sN ~ dexp(0.5) # life without transplant (mean 2)
      oT ~ dbern(0.8) # survive operation (prob 0.8)
      sP ~ dexp(0.2) # life if survive transplant (mean 5)
      sT <- oT*sP # total life time if choose transplant
      Ic <- 20000 + 3000*sT # total additional cost of transplant
      Is <- sT - sN # total additional survival
      r <- Ic/Is # individual cost per additional year
   }

   Output:
            Mean        SD  Naive SE Time-series SE
   Ic  3.197e+04 1.466e+04 1.639e+01      1.641e+01
   Is  1.991e+00 5.281e+00 5.905e-03      5.913e-03
   oT  8.005e-01 3.996e-01 4.468e-04      4.476e-04
   r  -2.197e+05 1.508e+08 1.686e+05      1.686e+05
   sN  1.999e+00 1.997e+00 2.233e-03      2.233e-03
   sP  4.982e+00 4.983e+00 5.571e-03      5.577e-03
   sT  3.990e+00 4.887e+00 5.464e-03      5.471e-03
   """

   Cf ~/blog/bugs_book_2_6_1.blog
      ~/webppl/bugs_book_2_6_1.wppl

   This model:
Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64

          oT    0.7971    0.4022     0.0020    0.0020   40004.4419    1.0000
          sN    2.0054    1.9993     0.0100    0.0096   40023.2213    1.0000
          sP    4.9685    4.9656     0.0248    0.0262   39325.3438    1.0000

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

          oT    0.0000    1.0000    1.0000    1.0000    1.0000
          sN    0.0493    0.5789    1.3914    2.7897    7.3861
          sP    0.1236    1.4235    3.4610    6.8997   18.2519

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function bugs_book_2_6_1()


    sN ~ Exponential(1/0.5) # life without transplant (mean 2)
    oT ~ Bernoulli(0.8)     # survive operation (prob 0.8)
    sP ~ Exponential(1/0.2) # life if survive transplant (mean 5)
    sT = oT*sP              # total life time if choose transplant
    Ic = 20000 + 3000*sT    # total additional cost of transplant
    Is = sT - sN            # total additional survival
    r  = Ic/Is              # individual cost per additional year

    # return r

end


model = bugs_book_2_6_1()

num_chains = 4

# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)
chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(
#                        # :alpha => Normal(2,sqrt(2)),
#                        # :beta => Normal(2,sqrt(2)),
#                        # :sigma => Gamma(2,2)
#                        ), MCMCThreads(), 40_000, num_chains)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)
# chains = sample(model, SMC(10_000), MCMCThreads(), 20_000, num_chains)
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)

# Both HMC and NUTS give the following error:
# ERROR: LoadError: TaskFailedException:
# TypeError: in typeassert, expected Float64, got a value of type ForwardDiff.Dual{Nothing,Float64,3}
# chains = sample(model, HMC(0.1,5), MCMCThreads(), 10_000, num_chains) # Error
# chains = sample(model, NUTS(0.65), MCMCThreads(), 10_000, num_chains) # Error

display(chains)
# display(plot(chains))

