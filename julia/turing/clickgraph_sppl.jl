#=

    This is a port of the SPPL model clickgraph.pynb
    """
    ClickGraph is an information-retrieval model used to compute the posterior probability that a pair 
    of items A and B are similar, conditioned on a user's access patterns to these items. This model is 
    used, for example, to measure the quality of a search engine, where A and B represent two URLs that 
    are returned to a given search query. A search engine is said to be good if the probability that 
    URLs A and B are similar is high.

    The probabilistic program below implements the ClickGraph model, for five pairs of URLs and one 
    user's click patterns.
    """

    This model:
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

         p_similar    0.6806    0.2866     0.0091    0.0320    88.3928    1.0026       16.3690


    * click_url_A_post (cf [1,1,0,1,0])
            parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
               Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

  click_url_A_post[1]    0.7950    0.4039     0.0128    0.0229   233.1889    0.9992       43.1831
  click_url_A_post[2]    0.7400    0.4389     0.0139    0.0355   274.5914    1.0121       50.8503
  click_url_A_post[3]    0.2370    0.4255     0.0135    0.0197   470.6780    0.9999       87.1626
  click_url_A_post[4]    0.7170    0.4507     0.0143    0.0235   376.7671    0.9990       69.7717
  click_url_A_post[5]    0.2600    0.4389     0.0139    0.0178   425.0068    0.9997       78.7050
   

    * click_url_B_post (cf [1,1,0,1,0])
           parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
               Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

  click_url_B_post[1]    0.6670    0.4715     0.0149    0.0226   267.1792    0.9990       44.5596
  click_url_B_post[2]    0.7630    0.4255     0.0135    0.0198   402.3125    0.9994       67.0968
  click_url_B_post[3]    0.2880    0.4531     0.0143    0.0180   410.6459    1.0010       68.4866
  click_url_B_post[4]    0.7230    0.4477     0.0142    0.0118   479.3376    0.9994       79.9429
  click_url_B_post[5]    0.2740    0.4462     0.0141    0.0207   513.0703    0.9991       85.5688

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function clickgraph_sppl(click_url_A=missing, click_url_B=missing) 
    if click_url_A === missing 
        n = 5
        click_url_A = tzeros(n)
        click_url_B = tzeros(n)
    else 
        n = length(click_url_A)
    end


    p_similar ~ Beta(1, 1)   # Prior probabilty URLs A and B are similar. (in [0,1])

    # Five file pairs
    
    similar         = tzeros(n)    # Are url_A[i] and url_B[i] similar? (binary)
    p_click_url_A   = tzeros(n)    # Probability user clicks url_A[i]. (in [0,1])
    p_click_url_B   = tzeros(n)    # Probability user clicks url_B[i]. (in [0,1])
    # click_url_A     = tzeros(n)    # Did user click url_A[i]? (binary)
    # click_url_B     = tzeros(n)    # Did user click url_B[i]? (binary)
    
    for i in 1:n
        similar[i] ~ Bernoulli(p_similar)
    
        if similar[i] == true
            p_click_url_A[i] ~ Uniform(0,1)
            p_click_url_B[i] ~ Dirac(p_click_url_A[i])
        else
            p_click_url_A[i] ~ Uniform(0,1)
            p_click_url_B[i] ~ Uniform(0,1)
        end
    
        click_url_A[i] ~ Bernoulli(p_click_url_A[i])
        click_url_B[i] ~ Bernoulli(p_click_url_B[i])
    end

    click_url_A_post = tzeros(n)
    click_url_B_post = tzeros(n)
    for i in 1:n
        click_url_A_post[i] ~ Bernoulli(p_click_url_A[i])
        click_url_B_post[i] ~ Bernoulli(p_click_url_B[i])
    end
end 


click_url_A = [1,1,0,1,0]
click_url_B = [1,1,0,1,0]
model = clickgraph_sppl(click_url_A, click_url_B)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  1_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

display(group(chns,:click_url_A_post))
display(group(chns,:click_url_B_post))

println("\nPredict:")
mpred = clickgraph_sppl(missing,missing)
pred = predict(mpred, chns)

display(pred)