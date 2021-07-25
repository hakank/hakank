#=
   From BLOG example/galaxy.blog

   numCluster: 10.0817
   Distributions of variable origCluster1 (num:0)
   2.00000 =>    1149  (0.114900)
   1.00000 =>    1133  (0.113300)
   4.00000 =>    1108  (0.110800)
   3.00000 =>    1090  (0.109000)
   5.00000 =>     990  (0.099000)
   6.00000 =>     969  (0.096900)
   7.00000 =>     845  (0.084500)
   8.00000 =>     771  (0.077100)
   9.00000 =>     586  (0.058600)
   10.00000 =>     436  (0.043600)
   11.00000 =>     339  (0.033900)
   12.00000 =>     242  (0.024200)
   13.00000 =>     152  (0.015200)
   14.00000 =>      87  (0.008700)
   15.00000 =>      52  (0.005200)
   16.00000 =>      31  (0.003100)
   17.00000 =>      11  (0.001100)
   18.00000 =>       5  (0.000500)
   19.00000 =>       3  (0.000300)
   20.00000 =>       1  (0.000100)

   g1: 27623.544339264394
   g6: 27443.067174186628
   g10: 27617.655492791007
   g15: 27490.9282783939
   g16: 27577.219002335823
   g32: 27520.492984190976
   g51: 27606.068946048606
   g71: 27560.49783845427
   g81: 27531.756338358115

   Cf ~/webppl/galaxy.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function galaxy(data)
    n = length(data)
    
    numCluster ~ Truncated(Poisson(10),1,Inf)
    clusVelocity ~ filldist(Uniform(5000.0, 50000.0),n)
    origCluster ~ filldist(DiscreteUniform(1,numCluster),n)

    velocity = tzeros(n)
    for i in 1:n
        velocity[i] ~ Normal(clusVelocity[origCluster[i]], sqrt(10000))
    end

    for i in 1:n
       data[i] ~ Dirac(velocity[i])
    end

    origCluster1 ~ Dirac(origCluster[1])
    g1 ~ Dirac(clusVelocity[origCluster[1]])
    g6 ~ Dirac(clusVelocity[origCluster[6]])
    g10 ~ Dirac(clusVelocity[origCluster[10]])
    g15 ~ Dirac(clusVelocity[origCluster[15]])
    g16 ~ Dirac(clusVelocity[origCluster[16]])
    g32 ~ Dirac(clusVelocity[origCluster[32]])
    g51 ~ Dirac(clusVelocity[origCluster[51]])
    g71 ~ Dirac(clusVelocity[origCluster[71]])
    g81 ~ Dirac(clusVelocity[origCluster[81]])

end

data = [9172,9350,9483,9558,9775,10227,10406,16084,16170,18419,18552,18600,18927,19052,
        19070,19330,19343,19349,19440,19473,19529,19541,19547,19663,19846,19856,19863,
        19914,19918,19973,19989,20166,20175,20179,20196,20215,20221,20415,20629,20795,
        20821,20846,20875,20986,21137,21492,21701,21814,21921,21960,22185,22209,22242,
        22249,22314,22374,22495,22746,22747,22888,22914,23206,23241,23263,23484,23538,
        23542,23666,23706,23711,24129,24285,24289,24366,24717,24990,25633,26960,26995,
        32065,32789,34279];
println("len(data): ", length(data))
model = galaxy(data)

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

println("numCluster: ", mean_val(chns,:numCluster))
show_var_dist_pct(chns,:numCluster)
show_var_dist_pct(chns,:origCluster1)

println("g1: ", mean_val(chns,:g1))
println("g6: ", mean_val(chns,:g6))
println("g10: ", mean_val(chns,:g10))
println("g15: ", mean_val(chns,:g15))
println("g16: ", mean_val(chns,:g16))
println("g32: ", mean_val(chns,:g32))
println("g51: ", mean_val(chns,:g51))
println("g71: ", mean_val(chns,:g71))
println("g81: ", mean_val(chns,:g81))
