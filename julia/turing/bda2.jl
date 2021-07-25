#=
  From the WebPPL model https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-data.md
  """
  Posterior prediction and model checking

  One important use of posterior predictive distributions is to examine the descriptive 
  adequacy of a model. The posterior predictive can be viewed as a set of predictions about 
  what data the model expects to see, based on the posterior distribution over parameters. 
  If these predictions do not match the data already seen, the model is descriptively inadequate.

  Let's say we ran 2 experiments that we believe are conceptually the same (e.g., asking 
  10 people whether or not they would vote for "Hillary Clinton or Donald Trump" and asking 
  a separate group of 10 people if they would vote for "Donald Trump or Hillary Clinton"). 
  Suppose we observed the following data from those 2 experiments: k1=0; k2=10.
  """
=#

using Turing
include("jl_utils.jl")

@model function bda2(k1=0,n1=10, k2=10,n2=10)
    
    # Sample rate from Uniform distribution
    p ~ Uniform(0,1)
    
    k1 ~ Binomial(n1, p)
    k2 ~ Binomial(n2, p)    

    # sample from binomial with updated p
    prior_p ~ Uniform(0,1)
    posteriorPredictive1 ~ Binomial(n1,p)
    posteriorPredictive2 ~ Binomial(n2,p)
       
end

# data
k1=0
n1=10
k2=10
n2=10
println("k1:$(k1) n1:$(n1) k2:$(k2) n2:$(n2)")
model = bda2(k1,n1,k2,2)

# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, 4)

display(chains)

show_var_dist_pct(chains, :posteriorPredictive1)
show_var_dist_pct(chains, :posteriorPredictive2)
