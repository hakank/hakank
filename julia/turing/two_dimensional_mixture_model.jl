#=

    This is a port of the SPPL model two-dimensional-mixture-model.pynb

    The SPPL model returns the following probabilities for these 
    conditions  
                (((-4 < X) < 4) & ((1 < Y) < 2)) 
                (((-4 < X) < 4) & ((-2 < Y) < -1)) 
                (((-1 < X) < 1) & ((-1 <= Y) <= 1)) 
                (((-1 < X) < 1) & ((2 <= Y) < 6))
    prior:
    [0.09278584524638006, 0.04331568961023601, 0.06717622976807626, 0.023365316112895024]
    posterior
    [0.40939191677247666, 0.1911185175795767, 0.29639656127801434, 0.10309300436993234]

    They corresponds to t1..t4 in the Turing.jl model.

    This model:

    * Prior()
        Summary Statistics
        parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

                 X    0.0233    2.0143     0.0201    0.0200    9920.7405    1.0004    86267.3088
           cluster    1.3992    0.4898     0.0049    0.0048   10323.7943    0.9999    89772.1241
                 Y    4.7994    4.1470     0.0415    0.0409   10318.3909    0.9999    89725.1381
                t1    0.0917    0.2886     0.0029    0.0030    9980.2571    1.0000    86784.8442
                t2    0.0378    0.1907     0.0019    0.0018    9026.5068    0.9999    78491.3637
                t3    0.0716    0.2578     0.0026    0.0028    9501.1301    1.0002    82618.5228
                t4    0.0233    0.1509     0.0015    0.0014    9867.5527    0.9999    85804.8059


    * PG(5)
        Summary Statistics
        parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

                 X   -0.0303    1.4162     0.0142    0.0325   1748.1662    1.0006      142.2777
           cluster    1.9790    0.1434     0.0014    0.0034   1897.8769    1.0001      154.4622
                 Y   -0.0137    1.7171     0.0172    0.0405   1998.9606    1.0007      162.6891
                t1    0.4232    0.4941     0.0049    0.0111   1950.7745    0.9999      158.7674
                t2    0.1776    0.3822     0.0038    0.0093   1874.7156    1.0005      152.5772
                t3    0.2957    0.4564     0.0046    0.0100   1768.5983    1.0004      143.9406
                t4    0.1035    0.3046     0.0030    0.0068   1948.0961    1.0001      158.5494

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

#=
# SPPL
X ~= norm(loc=0, scale=2)
Y ~= 0.6*norm(loc=8, scale=1) | 0.4*gamma(loc=-3, a=3)
=#
# It seems that 0.6 and 0.6 are the weights, i.e. 
# the probability of each cluster 
#
@model function two_dimensional_mixture_model() 
    X ~ Normal(0, 2)
    
    # Probability of the clusters
    cluster ~ Categorical([0.6,0.4]) # [cluster1, cluster2]
    if cluster == 1 
        Y ~ Normal(8, 1) 
    else 
        Y ~ Gamma(3) - 3
    end
     

    true ~ Dirac( ((-4 < X < 4) && (1 < Y^2 < 4)) 
                    || 
                  ((-1 < X < 1) && (-1.5 < Y < 6))
    )

    # The different conditions in the SPPL model
    t1 ~ Dirac((-4 < X < 4) && (-2 <  Y < -1)) 
    t2 ~ Dirac((-4 < X < 4) && ( 1 <  Y <  2)) 
    t3 ~ Dirac((-1 < X < 1) && (-1 <= Y <= 1)) 
    t4 ~ Dirac((-1 < X < 1) && ( 2 <= Y <  6))
    
    return [cluster,X,Y]

end 

model = two_dimensional_mixture_model()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  10_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq,20)
