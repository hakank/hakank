#=

    This is a port of the SPPL model simple-mixture-model.pynb


    This model:
    Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

            c    2.0791    0.8793     0.0088    0.0128   5795.0955    1.0002     1832.7310
            X   -0.3398    3.2100     0.0321    0.0466   5771.6388    1.0003     1825.3127

    Quantiles
    parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
        Symbol   Float64   Float64   Float64   Float64   Float64 

            c    1.0000    1.0000    2.0000    3.0000    3.0000
            X   -5.5343   -3.8297    0.2008    2.4301    4.4890

    Distributions of variable (num:20)
    [3.0, 1.9547181301360044]       =>      6 (0.0006)
    [2.0, 0.07355943510153802]      =>      6 (0.0006)
    [2.0, 0.019518361567171974]     =>      6 (0.0006)
    [1.0, -4.337533841906124]       =>      6 (0.0006)
    [3.0, 2.8480804714644505]       =>      6 (0.0006)
    [1.0, -5.5754239823735094]      =>      6 (0.0006)
    [3.0, 2.373924572733515]        =>      6 (0.0006)
    [1.0, -4.078683827279246]       =>      6 (0.0006)
    [2.0, 0.09106025572086306]      =>      5 (0.0005)
    [1.0, -5.650195069927168]       =>      5 (0.0005)
    [3.0, 4.157366712140572]        =>      5 (0.0005)
    [2.0, 0.02940438997958411]      =>      5 (0.0005)
    [3.0, 2.5010061236862846]       =>      5 (0.0005)
    [3.0, 2.9856046575410877]       =>      5 (0.0005)
    [3.0, 2.337433779685608]        =>      5 (0.0005)
    [2.0, 0.030036163336332175]     =>      5 (0.0005)
    [1.0, -3.8094485818954054]      =>      5 (0.0005)
    [3.0, 2.437511718049071]        =>      5 (0.0005)
    [3.0, 3.143030915985073]        =>      5 (0.0005)
    [3.0, 2.9811397850882315]       =>      5 (0.0005)

    Cluster 1:
    Summary Stats:
    Length:         3501
    Missing Count:  0
    Mean:           -4.347582
    Minimum:        -5.997945
    1st Quartile:   -4.912907
    Median:         -4.331252
    3rd Quartile:   -3.743788
    Maximum:        -3.001262


    Cluster 2:
    Summary Stats:
    Length:         2207
    Missing Count:  0
    Mean:           0.162009
    Minimum:        0.000159
    1st Quartile:   0.064479
    Median:         0.138028
    3rd Quartile:   0.239680
    Maximum:        0.731944


    Cluster 3:
    Summary Stats:
    Length:         4292
    Missing Count:  0
    Mean:           2.671340
    Minimum:        0.000234
    1st Quartile:   1.913621
    Median:         2.627983
    3rd Quartile:   3.438827
    Maximum:        6.290527

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

#=
# SPPL model
X ~=  .525 * norm(loc=-8, scale=2) \
    | .100 * norm(loc=0, scale=2) \
    | .375 * norm(loc=7, scale=3)
=#
@model function simple_mixture_model() 

    # The clusters (prior: uniform distribution of the clusters)
    c ~ Categorical([1/3,1/3,1/3]) 
    if c == 1
        X ~  0.525 * Normal(-8, 2) 
    elseif c == 2 
        X ~ 0.100 * Normal(0, 2) 
    else  
        X ~ 0.375 * Normal(7, 3)
    end

    # Condition
    true ~ Dirac( (0 < X < 10) || (-6 < X < -3) )

    return [c,X]
end 

model = simple_mixture_model()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq,20)

# Statistics for the three clusters
for c in 1:3
    println("\nCluster $c:")
    display([t[2] for t in genq if t[1] == c] |> summarystats)
end