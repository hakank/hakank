#=
  Bayesian Data Analysis of Wine Quality dataset

  I'm continue to testingMvNormal models to see how well it does on 
  various datasets.

  From http://archive.ics.uci.edu/ml/datasets/Wine+Quality
  """
  Data Set Information:

  The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. For more details, consult: [Web Link] or the reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.).

  These datasets can be viewed as classification or regression tasks. The classes are ordered and not balanced (e.g. there are many more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods.


  Attribute Information:

  For more information, read [Cortez et al., 2009].
  Input variables (based on physicochemical tests):
  1 - fixed acidity
  2 - volatile acidity
  3 - citric acid
  4 - residual sugar
  5 - chlorides
  6 - free sulfur dioxide
  7 - total sulfur dioxide
  8 - density
  9 - pH
  10 - sulphates
  11 - alcohol
  Output variable (based on sensory data):
  12 - quality (score between 0 and 10)
  """

  Dataset: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

  The datasets are in the files wine_quality-white.csv and winequality-red.csv. 
  Here I use the dataset for white wines, and will see the problem as a 
  classification problem.

  Summary for white wines:

  Num correct: 2622.0
  Num incorrect: 2276.0
  Accuracy: 0.5353205389955084
  Confusion matrix:
  7×7 Matrix{Float64}:
  0.0  0.0    8.0    11.0    1.0  0.0  0.0
  0.0  0.0    0.0     0.0    0.0  0.0  0.0
  0.0  0.0  673.0   935.0   12.0  0.0  0.0
  0.0  0.0  277.0  1757.0  164.0  0.0  0.0
  0.0  0.0   19.0   669.0  192.0  0.0  0.0
  0.0  0.0    1.0   115.0   59.0  0.0  0.0
  0.0  0.0    0.0     2.0    3.0  0.0  0.0
  num bad: 2364

  Real bad: when the difference between true and predicted >= 2:
  Number of real bad predictions (diff >= 2): 245  (0.0500204164965292%)

  Summary for red wines:
Predictions:
Num correct: 982.0
Num incorrect: 617.0
Accuracy: 0.6141338336460288
Confusion matrix:
7×7 Matrix{Float64}:
 0.0  0.0    9.0    1.0   0.0  0.0  0.0
 0.0  0.0    0.0    0.0   0.0  0.0  0.0
 0.0  0.0  518.0  214.0   2.0  0.0  0.0
 0.0  0.0  183.0  433.0  22.0  0.0  0.0
 0.0  0.0    6.0  162.0  31.0  0.0  0.0
 0.0  0.0    0.0   11.0   7.0  0.0  0.0
 0.0  0.0    0.0    0.0   0.0  0.0  0.0
num bad: 653

Real bad: when the difference between true and predicted >= 2:
Number of real bad predictions (diff >= 2): 47  (0.029393370856785492%)

=#
using Turing, StatsPlots, Distributions
using DataFrames, StatsBase
using CSV
include("jl_utils.jl")

#
# Read dataset and preprocessing
#
df = CSV.read("winequality-white.csv",DataFrame,header=true)
# df = CSV.read("winequality-red.csv",DataFrame,header=true)

display(describe(df))
num_rows,num_cols = size(df)
data = Matrix(df)
x = data[:,1:end-1]
y = data[:,end]

categories = unique(y) |> sort 
println("Mean value for each categories: ", categories)
display(categories .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")

# Standardize the dataset (better and faster)
# x = StatsBase.standardize(UnitRangeTransform,x) # This give very bad result
means = mean(x,dims=1)
stds = std(x,dims=1)
maxes = [maximum(x[:,i]) for i in 1:size(x)[2]]
x = (x .- means) ./ stds
# x = (x .- means) ./ maxes'

println("mean(x): ", mean(x,dims=1))
println("std(x): ", std(x,dims=1))

@model function wine_quality_model(x,y=missing, ::Type{T} = Float64) where {T}
    n,p = size(x)  

    if y === missing 
      y = Vector{T}(undef, n) 
    end

    a ~ Normal(0,1)
    params ~ filldist(Normal(0,1),p)  
    sigma ~ Uniform(0,1)

    # for i in 1:n 
    #   mu = a .+ dot(x[i,:], params)
    #   y[i] ~ Normal(mu,sigma)
    # end

    mu = a .+ x*params
    y ~ MvNormal(mu, sigma)

    return y

end

model = wine_quality_model(x,y)

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

chns_orig = chns
display(chns)


# display(plot(chns))
println("\nPredictions:")

# mpred = iris_model(x[50:51,:],missing)
mpred = wine_quality_model(x,missing)
pred = predict(mpred,chns)
# display(pred)


cats = Dict(3=>1,4=>3,5=>3,6=>4,7=>5,8=>6,9=>7)
confusion, bads = confusion_matrix(y,pred,cats,false)
println("Confusion matrix:")
display(confusion)
# println("\nBad classfied:")
# display(bads)
println("num bad: $(length(bads))") 
println("\nReal bad: when the difference between true and predicted >= 2:")
num_real_bad = 0
for (b1,b2) in bads 
  if abs(b2[1]-b2[2]) >= 2
    # println("b21:$(b2[1]) b22:$(b2[2])") 
    global num_real_bad += 1
  end 
end
println("Number of real bad predictions (diff >= 2): $num_real_bad  ($(num_real_bad / length(y))%)")

# println("\nbads: credible_intervals")
# for b in bads
#   println("b:",b)
#   ix = b[1]
#   println("x[$ix]: $(x[ix,:])  true cat (y[$ix]): $(y[ix])  (pred mean: ", mean(pred["y[$ix]"]))
#   # credible_interval(pred,"y[$(b[1])]",0.3)  
# end

# println("\nMean value for each category: ", 1:3)
# display(1:3 .|> s -> (s,mean(x[y.==s,:], dims=1)))
# println("\n")
