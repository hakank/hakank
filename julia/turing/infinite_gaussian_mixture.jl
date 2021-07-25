#=
  From
  Yi Wu, Lei Li, Stuart Russell, Rastislav Bodik
  "Swift: Compiled Inference for Probabilistic Programming Languages"
  Page 3

  Summary Statistics
  parameters      mean       std   naive_se       mcse         ess       rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64?    Float64?   Float64?      Float64? 

     cluster    3.5614    1.4721     0.0147     0.0347   1385.1836     1.0003      111.8346
       mu[1]   -0.1752    7.9247     0.0792     0.1902   1447.7438     1.0002      116.8855
       mu[2]    0.1621    8.1310     0.0813     0.2017   1361.4057     1.0007      109.9149
        z[1]    2.2715    1.2967     0.0130     0.0352   1192.9498     1.0004       96.3144
        z[2]    2.2670    1.2947     0.0129     0.0341   1242.9694     1.0001      100.3528
        z[3]    2.2901    1.2907     0.0129     0.0340   1366.0522     1.0006      110.2900
   x_post[1]    0.0905    1.4039     0.0140     0.0223   4274.1990     1.0006      345.0831
   x_post[2]    0.0648    7.9544     0.0795     0.2009   1411.9730     1.0021      113.9975
   x_post[3]   -0.2165    8.3757     0.0838     0.2278   1344.0788     1.0010      108.5160
       mu[3]    0.1385    8.4502     0.0998     0.2856     missing    missing       missing
       mu[4]    0.1468    8.7843     0.1334     0.3392     missing    missing       missing
       mu[5]   -0.9117    8.9978     0.1853     0.5347     missing    missing       missing
       mu[6]   -0.1977    9.2914     0.2807     0.9323     missing    missing       missing
       mu[7]   -0.3155    8.8300     0.4176     0.6646     missing    missing       missing
       mu[8]    4.1820   12.8344     1.1005    missing     missing    missing       missing
       mu[9]    5.5301    9.7624     1.4240    missing     missing    missing       missing
      mu[10]    2.6899    2.7582     0.6501    missing     missing    missing       missing
   ...

  Distributions of variable cluster (num:0)
  3.00000 =>    2838  (0.283800)
  2.00000 =>    2825  (0.282500)
  4.00000 =>    1979  (0.197900)
  5.00000 =>    1262  (0.126200)
  6.00000 =>     649  (0.064900)
  7.00000 =>     311  (0.031100)
  8.00000 =>      89  (0.008900)
  9.00000 =>      29  (0.002900)
  10.00000 =>      18  (0.001800)


   cf ~/webppl/infinite_gaussian_mixture.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

#=
BLOG model:

distinct Data D[20]; // 20 data points
#Cluster ~ Poisson(4); // Number of clusters

random Real mu(Cluster c) ~ Gaussian(0.0,10.0); // Cluster mean
random Cluster z(Data d) ~ UniformChoice({c for Cluster c});

random Real x(Data d) ~ Gaussian(mu(z(d)), 1.0); // data


=#
@model function infinite_gaussian_mixture(x=[0.1],n=3)
    # n = length(x)
    
    cluster ~ Truncated(Poisson(3),1,Inf) # Number of clusters   
    mu ~ filldist(Normal(0,10),cluster) # Cluster mean

    # Number of clusters per data points
    z = tzeros(n)
    for i in 1:n
        z[i] ~ DiscreteUniform(1,cluster)
    end

    # Observe the data.
    # z[i] returns a float, hence the round
    for i in 1:length(x)
        zi = round(Int,z[i])
        x[i] ~ Normal(mu[zi], 1.0)
    end

    # Here we want n posterior data points
    x_post = tzeros(n)
    for i in 1:n
        zi = round(Int,z[i])
        x_post[i] ~ Normal(mu[zi], 1.0)
    end
    
    # x1 ~ Dirac(x_post[1])
    # x2 ~ Dirac(x_post[2])
    # x3 ~ Dirac(x_post[3])

end

# x=[0.1,0.1,0.1]
x = [0.1]
n = 3 # we want 3 predictions
model = infinite_gaussian_mixture(x,n)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:cluster)
