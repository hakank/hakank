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

  This version use different training and test sets.

  * training pct 0.75:
    Num correct: 40.0
    Num incorrect: 4.0
    Accuracy: 0.9090909090909091
    Confusion matrix:
    3×3 Matrix{Float64}:
    14.0   3.0  0.0
     0.0  17.0  1.0
     0.0   0.0  9.0
    num bad: 4

    Distribution of the categories (training dataset):
    2.0 : 53 (39.55223880597015%)
    3.0 : 39 (29.104477611940297%)
    1.0 : 42 (31.34328358208955%)

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
println("Num rows (full dataset): $num_rows")


data = Matrix(df)
x = data[:,2:num_cols]
y = data[:,1]

# training_pct = 0.90
training_pct = 0.75
# training_pct = 0.66
# training_pct = 0.33
# training_pct = 0.11
# training_pct = 0.11
# training_pct = 3/num_rows

x_train, x_test, y_train, y_test = split_train_test(x,y,training_pct)

num_train_rows = length(y_train)
num_test_rows = length(y_test)

println("Num train rows: $num_train_rows num_test_rows: $num_test_rows")


categories = unique(y) |> sort 
println("Mean value for each species: ", categories)
display(categories .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")

means_train = mean(x_train,dims=1)
stds_train = std(x_train,dims=1)
x_train = (x_train .- means_train) ./ stds_train

means_test = mean(x_test,dims=1)
stds_test = std(x_test,dims=1)
x_test = (x_test .- means_test) ./ stds_test


println("train: mean(x_train): ", mean(x_train,dims=1))
println("train: std(x_train): ", std(x_train,dims=1))

model = linear_regression_model(x_train,y_train)

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

mpred = linear_regression_model(x_test,missing)
pred = predict(mpred,chns)
display(pred)

cats = Dict(1=>1, 2=>2, 3=>3)
confusion, bads = confusion_matrix(y_test,pred,cats,false)
println("Confusion matrix:")
display(confusion)
println("num bad: $(length(bads))") 

println("\nDistribution of the categories (training dataset):")
for (k,v) in make_hash(y_train)
  println("$k : $v ($(100*v/num_train_rows)%)")
end
