#=

  Test of uniform draw in Turing.jl

  What I can see Turing.jl don't have a function to randomly draw 
  an element from a list, e.g. 
     uniformDraw([1,10,32,100])
  It use have DiscreteUniform() and Categorical()

  So I'm trying to simulating uniformDraw....

  Asked about this at Discourse: 
  # https://discourse.julialang.org/t/turing-jl-unformdraw-distribution/65123
  https://discourse.julialang.org/t/turing-jl-uniformdraw-distribution/65123


  Ah, it's there as DiscreteNonParametric()
  https://juliastats.org/Distributions.jl/stable/univariate/#Discrete-Distributions

  I was looking in an old documentation of Distributions.jl!

=#

using Turing
include("jl_utils.jl")

# Defined in jl_utils.jl
# UniformDraw(xs) = DiscreteNonParametric(xs, ones(length(xs)) ./ length(xs))


@model function uniformDrawTest(x,pcts)
    #=
    function uniformDraw(x)
        n = length(x)
        ix1 ~ DiscreteUniform(1,n)
        return x[ix1]
    end
    function uniformDraw(x,pcts)
        n = length(x)
        ix2 ~ Categorical(simplex(pcts))
        return x[ix2]
    end
    val1 ~ Dirac(uniformDraw(x))
    val2 ~ Dirac(uniformDraw(x,pcts))
    val3 ~ Dirac(uniformDraw(x))        
    =#
    val1 ~ UniformDraw(x)
    val2 ~ DiscreteNonParametric(x,simplex(pcts))
    val3 ~ UniformDraw(x)
    println([val1,val2,val3])
end

x = [1,10,32,100]
pcts = [1,2,3,4] # ones(length(x))
model = uniformDrawTest(x,pcts)

# chns = sample(model, Prior(), 100_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 100_000)

display(chns)
show_var_dist_pct(chns, :ix)
show_var_dist_pct(chns, :val1)
show_var_dist_pct(chns, :val2)
show_var_dist_pct(chns, :val3)

