#= 
  Bayesian Null hypothesis test in Turing.jl
  From https://mhtess.github.io/bdappl/chapters/04-hypothesisTesting.html
  (WebPPL model)
  """
  Let’s see an example: Consider our children helping study from the previous study. Rather than estimate the 
  propensity_to_help, we might want to perform a “Null Hypothesis” test: What is the probability that the behavior 
  observed in our experiment (that 15 out of 20 kids helped) could be explained by true randomness (i.e., 
  propensity_to_help = 0.5)?  
 
  ...
 
  Bayesian Null Hypothesis Testing requires you to specify both hypotheses: 
  It is not enough to define what the null hypothesis is. The alternative 
  hypothesis is intuitively M1:θ≠0.5\mathcal{M}_1: \theta \neq 0.5M1​:θ≠0.5, 
  but we must be more specific. One standard way of defining the null 
  hypothesis is to put say that the parameter is drawn from a uniform 
  distribution.
  """

  From the WebPPL model:
  Marginal:
    {"the_better_model":"alternative"} : 0.758
    {"the_better_model":"null"} : 0.242

  This Turing.jl model:
   null model: 1 alternative model: 2:
   Distributions of variable the_better_model (num:0)
   2.00000 =>    7664  (0.766400)
   1.00000 =>    2336  (0.233600)

=#

using Turing
include("jl_utils.jl")

@model function supermodel(k=15,n=20)
    null_model ~ Dirac(0.5)
    alternative_model ~ Uniform(0,1)

    # Pick one of the models
    null = 1
    alternative = 2
    the_better_model ~ DiscreteUniform(1,2) # Categorical([0.5,0.5])
    p ~ Dirac(the_better_model == null ? null_model : alternative_model)
    k ~ Binomial(n,p)

    
end

# data
k=15
n=20
println("We observe $(k) successes of $(n) attempts")
model = supermodel(k,n)
# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)

display(chains)


println("null model: 1 alternative model: 2")
show_var_dist_pct(chains,:the_better_model)

