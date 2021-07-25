#=
  From https://mhtess.github.io/bdappl/chapters/04-hypothesisTesting.html
  WebPPL model:
  """
  The research assistants

  Recall our example from the previous chapter about two research assistants 
  who returned wildly different results. We had the intuition that they were 
  doing something different from one another. Is there a way to quantify our 
  degree of belief in that proposition?

  We can take a model comparison approach, comparing a model where we posit a 
  single latent parameter (research assistants are doing / measuring the same 
  thing) vs. a model where we posit different parameters for the different 
  research assistants. The latter model has more flexibility, but it also probably 
  predicts the observed data better.
  """

  Note: The WebPPL model use AIS() for this comparison.
  Here we use SMC's logevidence. 
  PG() and IS() also has chains.logevidence, but HM() does not.

=#
using Turing
include("jl_utils.jl")


@model function model_comparison_simple(k1=0,n1=10,k2=10,n=10)
    p1 ~ Uniform(0,1)
    k1 ~ Binomial(n1,p1)
    p2 = p1
    k2 ~ Binomial(n2,p2)
end

#
# "Complex" model with separate p for Binomial
#
@model function model_comparison_complex(k1=0,n1=10,k2=10,n=10)
    p1 ~ Uniform(0,1)
    k1 ~ Binomial(n1,p1)
    p2 ~ Uniform(0,1)
    k2 ~ Binomial(n2,p2)
end

model = model_comparison_simple()
# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, 4)
# display(chains)
display(chains)
ll1 = chains.logevidence


model = model_comparison_complex()
# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, 4)
# display(chains)
display(chains)
ll2 = chains.logevidence


println("\nlog evidence model 1: ", ll1)
println("log evidence model 2: ", ll2)
println("diff exp(ll2-ll1): ", exp(ll2-ll1))


