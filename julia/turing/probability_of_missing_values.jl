#=
  From https://github.polettix.it/ETOOBUSY/2021/07/19/brute-force-puzzle/
  """
  Consider a display with N digits in base b

      A random value id shown on the display; each possible value is equiprobable. 
      What is the expected number of missing digit values?

  As an example, suppose that we consider base-4 digits (i.e. 0 to 3) and a display that 
  has 6 slots. An example value shown on the display would be 232133, which includes digits 1, 2, 
  and 3 but not digit 0. In this arrangement, digit 0 is considered missing. Similarly, in 
  arrangement 111111 there are three missing digits, namely 0, 2, and 3.

  ....

  [By hand and Brute force:]
  2916 / 4096 ≅ 0.711914  
  """

  This Turing.jl model:
  """
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
       Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 
  .... 
  numMissing    0.7188    0.6277     0.0063    0.0079   6612.2186    0.9999      943.1206

  ...
  Distributions of variable numMissing (num:0)
  1.00000 =>    5291  (0.529100)
  0.00000 =>    3764  (0.376400)
  2.00000 =>     938  (0.093800)
  3.00000 =>       7  (0.000700)
  """

  Cf the WebPPL model probability_of_missing_values.wppl which give the exact probability
  0.7119140625000009

=#

using Turing
include("jl_utils.jl")

@model function probability_of_missing_values(base=4,n=6)
    # digit = tzeros(n)
    # for i in 1:n
    #     digit[i] ~ DiscreteUniform(0,base-1)
    # end
    # Faster:
    digit ~ filldist(DiscreteUniform(0,base-1), n)
    missingValues = tzeros(base)
    for i in 0:base-1
        missingValues[i+1] ~ Dirac(sum([digit[j] == i for j in 1:n]) == 0)
    end
    numMissing ~ Dirac(sum(missingValues))
end

base = 4
n = 6
model = probability_of_missing_values(base,n)

num_chns = 4
# chns = sample(model, Prior(), 1000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, IS(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, SGLD(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:numMissing)
