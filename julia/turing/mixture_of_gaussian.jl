#=
   From BLOG example/mixture-of-gaussian.blog
   """
   A model for mixture of Gaussian distribution. 
   The latent variable \verb|z| determines the mixture component of \verb|x|. 
   The program asks the possible value of \verb|z| given the evidence of \verb|x| being 0.2.
   
   @author leili
   @since 2014-06-16
   """

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           z    0.5481    0.4977     0.0050    0.0047   8954.7436    1.0004     1230.7234
       xpost    0.0560    1.1241     0.0112    0.0108   8908.5957    0.9999     1224.3809


   Cf ~/webppl/mixture_of_gaussian.wppl

=#

using Turing, StatsPlots

include("jl_utils.jl")

@model function mixture_of_gaussian(x=0.2)
    z ~ flip(0.5)
    x ~ z == true ? Normal(0.5, 1.0) : Normal(-0.5, 1.0)

    # Posterior prediction of x
    xpost ~ z == true ? Normal(0.5, 1.0) : Normal(-0.5, 1.0)
end

x=0.2
model = mixture_of_gaussian(x)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))



