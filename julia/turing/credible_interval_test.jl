#=
   Test of credible interval in Turing.jl

   This is a port of the WebPPL model in 
   https://mhtess.github.io/bdappl/chapters/03-simpleModels.html
   """
   A simple generative process
   Imagine we are investigating the origins of prosocial behavior: Some scientists believe humans are 
   born with a self-interested tendency and only acquire prosociality through instruction (perhaps mediated 
   by language). Others believe altruism in innate; as soon as children are physically and cognitively 
   able to help, they will. To investigate this, we are going to put pre-verbal infants in a context where 
   a person is struggling to complete their goal (e.g., as in this classic study 
   [https://www.youtube.com/watch?v=kfGAen6QiUE Toddler Altruism]).

   We plan to conduct this experiment on 20 infants. Each child may or may not help; what we are interested 
   in is the population’s propensity to help. A simple generative model of this task would be that infants' 
   propensity to help is like the bias of a coin, and whether or not they help in the moment is the 
   result of flipping that coin.
   """

   credible_interval is defined in jl_utils.jl 


   Output of the model:
"""
Summary Statistics
                parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                    Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

         priorDistribution    0.4966    0.2861     0.0029    0.0054   3065.5383    1.0000     1283.7262
        propensity_to_help    0.7260    0.0937     0.0009    0.0013   5215.2559    0.9999     2183.9430
       posteriorPredictive   14.5179    2.6933     0.0269    0.0323   6745.5678    1.0000     2824.7771
  prior_propensity_to_help    0.4966    0.2861     0.0029    0.0054   3065.5383    1.0000     1283.7262
           priorPredictive    9.9341    6.0131     0.0601    0.1088   3370.6685    0.9999     1411.5027

Quantiles
                parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
                    Symbol   Float64   Float64   Float64   Float64   Float64 

         priorDistribution    0.0203    0.2559    0.4953    0.7390    0.9736
        propensity_to_help    0.5260    0.6663    0.7309    0.7937    0.8867
       posteriorPredictive    9.0000   13.0000   15.0000   17.0000   19.0000
  prior_propensity_to_help    0.0203    0.2559    0.4953    0.7390    0.9736
           priorPredictive    0.0000    5.0000   10.0000   15.0000   20.0000

Credible interval for posteriorPredictive with mass 0.9: (10.000000..18.000000)
Credible interval for posteriorPredictive with mass 0.95: (9.000000..19.000000)
Credible interval for posteriorPredictive with mass 0.99: (8.000000..20.000000)
Credible interval for posteriorPredictive with mass 0.1: (13.000000..13.000000)
Credible interval for posteriorPredictive with mass 0.01: (9.000000..9.000000)
"""

=#

using Turing
using Random
include("jl_utils.jl")


@model function credible_interval_test(k=15,n=20)
    priorDistribution ~ Uniform(0,1)
    
    # propensity to help, or true population proportion who would help
    propensity_to_help ~ Uniform(0,1)
    
    # Observed k children who help
    # Assuming each child's response is independent of each other
    k ~ Binomial(n, propensity_to_help)

    # predict what the next n will do
    posteriorPredictive ~ Binomial(n, propensity_to_help)

    # duplicate model structure and parameters but omit observe
    prior_propensity_to_help ~ Dirac(priorDistribution)
    priorPredictive ~ Binomial(n, prior_propensity_to_help)


end

# observed data
k = 15 # number of children who help
n = 20 # total number of children
model = credible_interval_test(k,n)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, IS(), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, 4)

display(chns)

credible_interval(chns, "posteriorPredictive",0.90)
credible_interval(chns, "posteriorPredictive",0.95)
credible_interval(chns, "posteriorPredictive",0.99)
credible_interval(chns, "posteriorPredictive",0.1)
credible_interval(chns, "posteriorPredictive",0.01)
