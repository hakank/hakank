#=
  ClickGraph example from PSI I paper, page 4

  Where the objective is to determine how similar the 
  clicks of A and B are.


  From the PSI model
    // Orig: E[simAll]: 14475/23569 (~0.6141)
    clicksA := [1,1,1,0,0];
    clicksB := [1,1,1,0,0];
    
    // Variant: E[simAll]: 10581/38342 (~0.2759636)
    clicksA := [1,1,1,0,1,1,1,0];
    clicksB := [0,0,0,1,0,0,0,1];


   This Turing.jl model:
   Variant 1 ("very similar")  : simAll    0.6159 
   Variant 2 ("very different"): simAll    0.2389

  cf ~/psi/clickGraph_psi1.psi
     ~/blog/clickGraph_psi1.blog
     ~/webppl/clickGraph.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function clickGraph(clickAs,clickBs)
    n = length(clickAs)

    simAll ~ Uniform(0,1)

    sim ~ filldist(flip(simAll),n)
    pA ~ filldist(Uniform(0,1),n)
    pB = tzeros(n)
    for i in 1:n
        if sim[i] == true
            pB[i] ~ Dirac(pA[i])
        else
            pB[i] ~ Uniform(0,1)
        end
    end
      
    for i in 1:n
        clickAs[i] ~ flip(pA[i])
        clickBs[i] ~ flip(pB[i])        
    end
   
end

# Very similar
# clickAs = [true,true,true,false,false]
# clickBs = [true,true,true,false,false]    

# Variant: completely different
clickAs = [true  ,true,true ,false,true ,true ,true ,false]
clickBs = [false,false,false,true ,false,false,false,true]
println("clickAs: ", clickAs)
println("clickBs: ", clickBs)
model = clickGraph(clickAs,clickBs)
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

