#=
  From the WebPPL model:https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-data.md
  """
  Here, we explore the result of an experiment with 15 trials and binary outcomes (e.g., flipping a coin with an 
  uncertain weight, asking people if they'll vote for Hillary Clinton or Donald Trump, ...)
  """
=#

using Turing
include("jl_utils.jl")

@model function bda(k=1,n=15)
    p ~ Uniform(0, 1)
    
    # Observed k number of successes, assuming a binomial
    k ~ Binomial(n, p)
    
    # sample from binomial with updated p
    posteriorPredictive ~ Binomial(n,p)
    
    # sample fresh p
    prior_p ~ Uniform(0,1)
    # sample from binomial with fresh p
    priorPredictive ~ Binomial(n,prior_p)
   
end

# data
k = 1  # number of successes
n = 15 # number of attempts

model = bda(k,n)

# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, IS(), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, 4)

display(chns)

show_var_dist_pct(chns, :priorPredictive)
show_var_dist_pct(chns, :posteriorPredictive)
