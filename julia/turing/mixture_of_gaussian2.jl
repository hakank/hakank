#=
  From BLOG examples/mixture-of-gaussian-full.blog

  Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           p    0.3237    0.3005     0.0030    0.0033   8144.0941    0.9999      464.8986
           a    0.1618    0.5440     0.0054    0.0063   8092.3273    0.9999      461.9436
           b    0.2979    0.4666     0.0047    0.0052   7836.0821    0.9999      447.3160
        z[1]    0.3249    0.4684     0.0047    0.0048   8429.8487    1.0000      481.2107
        z[2]    0.3224    0.4674     0.0047    0.0045   8398.4099    0.9999      479.4160
        z[3]    0.3204    0.4667     0.0047    0.0054   8177.2874    1.0000      466.7934
        z[4]    0.3247    0.4683     0.0047    0.0050   8229.2433    0.9999      469.7593
      min_ab   -0.0771    0.4745     0.0047    0.0058   7699.9809    0.9999      439.5468

  Cf ~/webppl/mixture_of_gaussian2.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

@model function mixture_of_gaussian2(x=[0.2,1.0,0.5,0.6])
    n = length(x)
    p ~ Beta(0.5, 1)
    
    a ~ Uniform(-1, 1)
    b ~ Uniform(-1, 1)
    z ~ filldist(flip(p), n)
    for i in 1:n
        if z[i] == true
            x[i] ~ Normal(a, 1.0)
        else 
            x[i] ~ Normal(b, 1.0)
        end
    end

   min_ab ~ Dirac(min(a,b))
    
end

x=[0.2,1.0,0.5,0.6]
model = mixture_of_gaussian2(x)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))



