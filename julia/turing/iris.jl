#=
  Bayesian Data Analysis of Iris dataset


5×7 DataFrame
 Row │ variable     mean     min     median  max        nmissing  eltype                          
     │ Symbol       Union…   Any     Union…  Any        Int64     DataType                        
─────┼────────────────────────────────────────────────────────────────────────────────────────────
   1 │ SepalLength  5.84333  4.3     5.8     7.9               0  Float64
   2 │ SepalWidth   3.05733  2.0     3.0     4.4               0  Float64
   3 │ PetalLength  3.758    1.0     4.35    6.9               0  Float64
   4 │ PetalWidth   1.19933  0.1     1.3     2.5               0  Float64
   5 │ Species               setosa          virginica         0  CategoricalValue{String, UInt8}
   
Mean value for each species: ["virginica", "setosa", "versicolor"]
 ("virginica", [6.587999999999999 2.9739999999999998 5.552 2.026])
 ("setosa", [5.006 3.428 1.462 0.24600000000000002])
 ("versicolor", [5.935999999999999 2.77 4.26 1.3259999999999998])

Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

           a    1.1509    0.2081     0.0066    0.0106   484.2115    1.0090       97.6036
        c[1]   -0.1040    0.0589     0.0019    0.0026   439.0116    0.9990       88.4926
        c[2]   -0.0396    0.0595     0.0019    0.0036   384.6719    1.0042       77.5392
        c[3]    0.2247    0.0559     0.0018    0.0026   391.6563    1.0045       78.9471
        c[4]    0.6119    0.0907     0.0029    0.0046   380.7559    1.0086       76.7498
       sigma    0.2206    0.0133     0.0004    0.0007   604.2689    0.9992      121.8038


parameter: credible_intervals
Credible interval for a with mass 0.95: (0.762098..1.562943)
Credible interval for c[1] with mass 0.95: (-0.228162..-0.000480)
Credible interval for c[2] with mass 0.95: (-0.155606..0.083093)
Credible interval for c[3] with mass 0.95: (0.112843..0.334041)
Credible interval for c[4] with mass 0.95: (0.442641..0.800271)
Credible interval for sigma with mass 0.95: (0.194433..0.247127)

Confusion matrix:
3×3 Matrix{Float64}:
 50.0   0.0   0.0
  0.0  48.0   2.0
  0.0   2.0  48.0

Bad classfied:
4-element Vector{Any}:
  71 => (2 => 3)
  84 => (2 => 3)
 120 => (3 => 2)
 134 => (3 => 2)
num bad: 4

bads: credible_intervals
b:71 => (2 => 3)
x[71]: [5.9, 3.2, 4.8, 1.8]  true cat (y[71]): 2.0  (pred mean: 2.5812725160974286
Credible interval for y[71] with mass 0.3: (2.481020..2.645030)
b:84 => (2 => 3)
x[84]: [6.0, 2.7, 5.1, 1.6]  true cat (y[84]): 2.0  (pred mean: 2.548502053149339
Credible interval for y[84] with mass 0.3: (2.419795..2.590571)
b:120 => (3 => 2)
x[120]: [6.0, 2.2, 5.0, 1.5]  true cat (y[120]): 3.0  (pred mean: 2.479493724261082
Credible interval for y[120] with mass 0.3: (2.419840..2.583439)
b:134 => (3 => 2)
x[134]: [6.3, 2.8, 5.1, 1.5]  true cat (y[134]): 3.0  (pred mean: 2.442839883396137
Credible interval for y[134] with mass 0.3: (2.289724..2.465010)

Mean value for each species: 1:3
3-element Vector{Tuple{Int64, Matrix{Float64}}}:
 (1, [5.006 3.428 1.462 0.24600000000000002])
 (2, [5.935999999999999 2.77 4.26 1.3259999999999998])
 (3, [6.587999999999999 2.9739999999999998 5.552 2.026])


=#
using Turing, StatsPlots, Distributions
using DataFrames,RDatasets
include("jl_utils.jl")

# Read dataset 
iris = dataset("datasets", "iris")

# Convert Categorical to integer
conv = Dict(
    "setosa" => 1,
    "versicolor" => 2,
    "virginica" => 3,
)
display(describe(iris))
iris2 = Matrix(transform(iris,[:Species] => ByRow(i->conv[i]))[:,[1,2,3,4,6]])
x = iris2[:,1:4]
y = iris2[:,5]

println("Mean value for each species: ", keys(conv))
display(keys(conv) .|> s -> (s,mean(x[iris.Species.==s,:], dims=1)))
println("\n")


@model function iris_model(x,y=missing, ::Type{T} = Float64) where {T}
    n,p = size(x)  

    if y === missing 
      y = Vector{T}(undef, n) 
    end

    a ~ Normal(0,1)
    # Parameters:
    # - sepalLength
    # - sepalWidth
    # - petalLength
    # - petalWidth
    params ~ filldist(Normal(0,5),p)  
    sigma ~ Uniform(0,1)

    # This works, but is slower (and not so neat)
    # for i in 1:n
    #     mu = a + c[1]*x[i,1] + c[2]*x[i,2] + c[3]*x[i,3] + c[4]*x[i,4]
    #     y[i] ~ Normal(mu,sigma)
    # end

    # Better (and neater)
    mu = a .+ x*params
    y ~ MvNormal(mu, sigma)

    return y

end

model = iris_model(x,y)

# println("Prior:")
# chns = sample(model, Prior(), 10_000)
# display(chns)

println("\nModel:")
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)
# chns = sample(model, HMC(0.1,6), 1_000)
chns = sample(model, NUTS(), 1_000)

display(chns)

println("\nparameter: credible_intervals")
for v in ["a", "params[1]", "params[2]", "params[3]", "params[4]","sigma"] 
  credible_interval(chns,v,0.95)
end


# display(plot(chns))
println("\nPredictions:")

# mpred = iris_model(x[50:51,:],missing)
mpred = iris_model(x,missing)
pred = predict(mpred,chns)
display(pred)

cats = Dict(1=>1, 2=>2, 3=>3)
confusion, bads = confusion_matrix(y,pred,cats,false)
println("Confusion matrix:")
display(confusion)
println("\nBad classfied:")
display(bads)
println("num bad: $(length(bads))") 
println("\nbads: credible_intervals")
for b in bads
  println("b:",b)
  ix = b[1]
  println("x[$ix]: $(x[ix,:])  true cat (y[$ix]): $(y[ix])  (pred mean: ", mean(pred["y[$ix]"]))
  credible_interval(pred,"y[$(b[1])]",0.3)  

end

println("\nMean value for each species: ", 1:3)
display(1:3 .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")
