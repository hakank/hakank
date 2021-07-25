#=

  From BLOG examples/mixture-of-gaussian-infinite.blog
  """
  Gaussian mixture model with infinite number of components
 
  @author leili
  @date 2014-07-10
  """

  Note: The BLOG model remove numComponent == 0 via a condition.
        Here we remove it using Truncated(Poisson(2),0,Inf)

  Summary Statistics
    parameters      mean       std   naive_se       mcse         ess       rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64?    Float64?   Float64?      Float64? 

  numComponent    2.2097    1.2555     0.0126     0.0179   4752.1622     0.9999     1866.5209
          z[1]    1.6089    0.9319     0.0093     0.0121   4800.4385     0.9999     1885.4825
          z[2]    1.6034    0.9092     0.0091     0.0128   5013.1564     0.9999     1969.0324
          z[3]    1.6042    0.9161     0.0092     0.0134   5163.1008     0.9999     2027.9265
          z[4]    1.5902    0.8956     0.0090     0.0136   4864.7405     1.0001     1910.7386
         mu[1]    0.2789    0.4861     0.0049     0.0064   5152.2290     1.0000     2023.6563
         mu[2]    0.2106    0.5170     0.0064     0.0090     missing    missing       missing
         mu[3]    0.1611    0.5371     0.0093     0.0134     missing    missing       missing
         mu[4]    0.1342    0.5243     0.0138     0.0170     missing    missing       missing
     x_post[1]    0.3065    1.1066     0.0111     0.0121   8080.3308     1.0000     3173.7356
     x_post[2]    0.3533    1.1055     0.0111     0.0116   8837.3254     1.0001     3471.0626
     x_post[3]    0.3147    1.0961     0.0110     0.0105   9327.8597     0.9999     3663.7312
     x_post[4]    0.3229    1.0999     0.0110     0.0120   8585.7202     0.9999     3372.2389
         mu[5]    0.0978    0.5476     0.0237     0.0348     missing    missing       missing
         mu[6]    0.0579    0.5407     0.0401    missing     missing    missing       missing
         mu[7]    0.0565    0.4821     0.0568    missing     missing    missing       missing
         mu[8]    0.3198    0.4958     0.0891    missing     missing    missing       missing
         mu[9]    0.1723    0.4504     0.2014    missing     missing    missing       missing


  Distributions of variable numComponent (num:0)
  1.00000 =>    3517  (0.351700)
  2.00000 =>    3139  (0.313900)
  3.00000 =>    1900  (0.190000)
  4.00000 =>     908  (0.090800)
  5.00000 =>     354  (0.035400)
  6.00000 =>     110  (0.011000)
  7.00000 =>      41  (0.004100)
  8.00000 =>      26  (0.002600)
  9.00000 =>       5  (0.000500)


  Cf ~/webppl/mixture_of_gaussian_infinite.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

@model function mixture_of_gaussian_infinite(x=[0.2,1.0,0.5,0.6])
    n = length(x)

    # Note: We have to set 0..Inf (not 1..Inf), otherwise
    # the value 1 will not be drawn.
    numComponent ~ truncated(Poisson(2),0,Inf)
    z ~ filldist(DiscreteUniform(1,numComponent),n)
    mu ~ filldist(Uniform(-1,1), numComponent)

    for i in 1:n
        x[i] ~ Normal(mu[z[i]],1.0)
    end

    x_post = tzeros(n) # Vector{Real}(undef, n) 
    for i in 1:n
        x_post[i] ~ Normal(mu[z[i]],1.0)
    end

end

x=[0.2,1.0,0.5,0.6]
model = mixture_of_gaussian_infinite(x)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :numComponent)
