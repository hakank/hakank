#=
  Bayesian Data Analysis of Wine dataset

  I'm continue to testingMvNormal models to see how well it does on 
  various datasets.

  From http://archive.ics.uci.edu/ml/datasets/Wine
  """ 
    Original Owners:

    Forina, M. et al, PARVUS -
    An Extendible Package for Data Exploration, Classification and Correlation.
    Institute of Pharmaceutical and Food Analysis and Technologies, Via Brigata Salerno,
    16147 Genoa, Italy.

    Donor:

    Stefan Aeberhard, email: stefan '@' coral.cs.jcu.edu.au

    Data Set Information:

    These data are the results of a chemical analysis of wines grown in the same region in Italy 
    but derived from three different cultivars. The analysis determined the quantities of 13 
    constituents found in each of the three types of wines.

    I think that the initial data set had around 30 variables, but for some reason I only have the 
    13 dimensional version. I had a list of what the 30 or so variables were, 
    but a.) I lost it, and b.), I would not know which 13 variables are 
    included in the set.

    The attributes are (dontated by Riccardo Leardi, riclea '@' anchem.unige.it )
    1) Alcohol
    2) Malic acid
    3) Ash
    4) Alcalinity of ash
    5) Magnesium
    6) Total phenols
    7) Flavanoids
    8) Nonflavanoid phenols
    9) Proanthocyanins
    10)Color intensity
    11)Hue
    12)OD280/OD315 of diluted wines
    13)Proline

    In a classification context, this is a well posed problem with "well behaved" class structures. 
    A good data set for first testing of a new classifier, but not very challenging.
  """

  The dataset is in the file wine.csv. The first entry is the class: 1, 2, 3 (Green,Red,Yellow)


  Summary:
  Num correct: 171.0
  Num incorrect: 7.0
  Accuracy: 0.9606741573033708
  Confusion matrix:
  3×3 Matrix{Float64}:
   56.0   3.0   0.0
    1.0  69.0   1.0
    0.0   2.0  46.0

  Bad classfied:
  7-element Vector{Any}:
    5 => (1 => 2)
    22 => (1 => 2)
    44 => (1 => 2)
    62 => (2 => 3)
    122 => (2 => 1)
    131 => (3 => 2)
    142 => (3 => 2)
  num bad: 7

  Note that this use the full dataset in training and testing. Comparing to 
  other algorithms (e.g. Weka's Logistic, etc they most result in 100% correct predictions. )
  
=#
using Turing, StatsPlots, Distributions
using DataFrames, StatsBase
using CSV
include("jl_utils.jl")

#
# Read dataset and preprocessing
#
df = CSV.read("wine.csv",DataFrame,header=false)

display(describe(df))
num_rows,num_cols = size(df)
data = Matrix(df)
x = data[:,2:num_cols]
y = data[:,1]

categories = unique(y) |> sort 
println("Mean value for each species: ", categories)
display(categories .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")

# Standardize the dataset (better and faster)
# x = StatsBase.standardize(UnitRangeTransform,x) # This give very bad result
means = mean(x,dims=1)
stds = std(x,dims=1)
maxes = [maximum(x[:,i]) for i in 1:size(x)[2]]
x = (x .- means) ./ stds
# x = (x .- means) ./ maxes'
# x = x ./ maxes'

println("mean(x): ", mean(x,dims=1))
println("std(x): ", std(x,dims=1))

@model function wine_model(x,y=missing, ::Type{T} = Float64) where {T}
    n,p = size(x)  

    if y === missing 
      y = Vector{T}(undef, n) 
    end

    a ~ Normal(0,1)
    params ~ filldist(Normal(0,1),p)  
    sigma ~ Uniform(0,1)

    mu = a .+ x*params
    y ~ MvNormal(mu, sigma)
    
    return y

end

model = wine_model(x,y)

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


# display(plot(chns))
println("\nPredictions:")

# mpred = iris_model(x[50:51,:],missing)
mpred = wine_model(x,missing)
pred = predict(mpred,chns)
display(pred)

cats = Dict(1=>1, 2=>2, 3=>3)
confusion, bads = confusion_matrix(y,pred,cats,true)
println("Confusion matrix:")
display(confusion)
println("\nBad classfied:")
display(bads)
println("num bad: $(length(bads))") 
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
