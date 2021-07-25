#=

  Murder mystery

  From
  Andy Gordpn:
  "Reverend Bayes, meet Countess Lovelace: Probabilistic Programming for Machine Learning"
  https://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2012/Reverend-Bayes-meet-Countess-Lovelace-Probabilistic-Programming-for-Machine-Learning
  Around @14:00

  """
  Miss Scarlett dunnit 30%. Col Mustard dunnit 70%.
  Scarlett uses gun 3%, uses pipe 97%.
  Mustard uses gun 80%, uses pipe 20%.
  ...
  We found a gun at the Scene.
  What is the probability that Scarlett dunnit?
  """

  * Original question: we found a gun, what is the
    probability that Scarlett did it?

   It's not very likely that Scarlett did it! 

   Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    scarlett    0.0142    0.1183     0.0012    0.0013   7957.8849    1.0001      605.3925
     mustard    0.7024    0.4572     0.0046    0.0057   7373.2272    1.0002      560.9150
     withGun    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
    withPipe    0.2031    0.4023     0.0040    0.0038   8323.5860    0.9999      633.2131


  * If we had found a pipe instead:

    It's a little more probable that Mustard did it. But Scarlett is still under
    investigation for it.

   Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    scarlett    0.6620    0.4731     0.0047    0.0059   6800.0219    1.0003      441.2161
     mustard    0.6979    0.4592     0.0046    0.0054   6727.5710    0.9999      436.5151
     withGun    0.2908    0.4542     0.0045    0.0060   6720.3716    1.0002      436.0480
    withPipe    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN


  * Here's the priors (after we just heard about the murder)
    Mustard probably did it and it's more likely that it was with a Gun than a knife.


   Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    scarlett    0.2923    0.4548     0.0045    0.0049   9712.4171    1.0002    33035.4322
     mustard    0.7020    0.4574     0.0046    0.0052   8739.3445    1.0002    29725.6616
     withGun    0.5762    0.4942     0.0049    0.0052   9747.4247    1.0000    33154.5059
    withPipe    0.4247    0.4943     0.0049    0.0046   9917.5952    0.9999    33733.3169


  cf ~/blog/murder_mystery.blog
     ~/psi/murder_mystery.psi
     ~/webppl/murder_mystery.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

@model function murder_mystery()
    scarlett ~ flip(0.30)
    mustard ~ flip(0.70)
    
    withGun ~ scarlett ? flip(0.03) : flip(0.80)
    withPipe ~ scarlett ? flip(0.97) : flip(0.20)
    
    true ~ Dirac(withGun == true)
    # true ~ Dirac(withPipe == true)
    # true ~ Dirac(withGun < withPipe)
    # true ~ Dirac(withGun > withPipe)    

end

model = murder_mystery()

# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)



display(chns)
# display(plot(chns))
