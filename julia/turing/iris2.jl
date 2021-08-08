#=
  Bayesian Data Analysis of Iris dataset

  This model uses a linear regression.

  Compared to iris.jl this program splits the data into a training and test set.
  Here are some results for different training percentages.

  * For train_pct 0.75
    Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

           a    1.0930    0.2342     0.0074    0.0087   383.1404    1.0002       90.8346
   params[1]   -0.0735    0.0656     0.0021    0.0029   352.8822    1.0033       83.6610
   params[2]   -0.0659    0.0661     0.0021    0.0031   404.0677    1.0012       95.7960
   params[3]    0.2289    0.0625     0.0020    0.0026   303.2456    1.0009       71.8932
   params[4]    0.5658    0.1042     0.0033    0.0036   374.0015    0.9993       88.6680
       sigma    0.2228    0.0155     0.0005    0.0005   630.9561    0.9995      149.5866

    Num correct: 37.0
    Num incorrect: 1.0
    Accuracy: 0.9736842105263158
    Confusion matrix:
    3×3 Matrix{Float64}:
    15.0   0.0  0.0
     0.0  14.0  0.0
     0.0   1.0  8.0
    num bad: 1
    Distribution of the training categories:
    1.0 : 35 (31.25%)
    2.0 : 36 (32.142857142857146%)
    3.0 : 41 (36.607142857142854%)

    * train pct 0.66    
    Num correct: 49.0
    Num incorrect: 2.0
    Accuracy: 0.9607843137254902
    Confusion matrix:
    3×3 Matrix{Float64}:
    18.0   0.0   0.0
    0.0  15.0   1.0
    0.0   1.0  16.0
    num bad: 2
    Distribution of the training categories:
    1.0 : 32 (32.323232323232325%)
    2.0 : 34 (34.343434343434346%)
    3.0 : 33 (33.333333333333336%)

    * train pct 0.066 (10 train instances)
    Num correct: 130.0
    Num incorrect: 10.0
    Accuracy: 0.9285714285714286
    Confusion matrix:
    3×3 Matrix{Float64}:
    46.0   0.0   0.0
     0.0  42.0   6.0
     0.0   4.0  42.0
    num bad: 10
    Distribution of the training categories:
    1.0 : 4 (40.0%)
    2.0 : 2 (20.0%)
    3.0 : 4 (40.0%)

    Not too bad.

    * train pct 0.02 (3 traingin instances)
    Interestingly, even with as few as 3 (of 150) training instances the 
    model might get quite good accuracy, but sometimes it's really bad, 
    e.g. when the training instances are of the same class. 
    Here are some runs with some different distributions of the three 
    instances:

    
    Num correct: 135.0
    Num incorrect: 12.0
    Accuracy: 0.9183673469387755
    Confusion matrix:
    3×3 Matrix{Float64}:
    49.0   0.0   0.0
     0.0  38.0  11.0
     0.0   1.0  48.0
    num bad: 12
    Distribution of the training categories:
    1.0 : 1 (33.333333333333336%)
    2.0 : 1 (33.333333333333336%)
    3.0 : 1 (33.333333333333336%)


    Num correct: 112.0
    Num incorrect: 35.0
    Accuracy: 0.7619047619047619
    Confusion matrix:
    3×3 Matrix{Float64}:
    49.0   0.0   0.0
     0.0  48.0   0.0
     0.0  35.0  15.0
    num bad: 35
    Distribution of the training categories:
    1.0 : 1 (33.333333333333336%)
    2.0 : 2 (66.66666666666667%)

    Num correct: 121.0
    Num incorrect: 26.0
    Accuracy: 0.8231292517006803
    Confusion matrix:
    3×3 Matrix{Float64}:
    48.0   0.0   0.0
     0.0  24.0  26.0
     0.0   0.0  49.0
    num bad: 26
    Distribution of the training categories:
    1.0 : 2 (66.66666666666667%)
    3.0 : 1 (33.333333333333336%)

    Num correct: 144.0
    Num incorrect: 3.0
    Accuracy: 0.9795918367346939
    Confusion matrix:
    3×3 Matrix{Float64}:
    50.0   0.0   0.0
     0.0  46.0   3.0
     0.0   0.0  48.0
    num bad: 3
    Distribution of the training categories:
    2.0 : 1 (33.333333333333336%)
    3.0 : 2 (66.66666666666667%)
        
    Num correct: 55.0
    Num incorrect: 92.0
    Accuracy: 0.3741496598639456
    Confusion matrix:
    3×3 Matrix{Float64}:
    47.0   0.0  0.0
    42.0   8.0  0.0
    20.0  30.0  0.0
    num bad: 92
    Distribution of the training categories:
    1.0 : 3 (100.0%)


    Cf iris.jl
=#
using Turing, StatsPlots, Distributions, StatsBase
using DataFrames,RDatasets
include("jl_utils.jl")

# Read dataset 
iris = dataset("datasets", "iris")

num_rows, num_cols = size(iris)

# train_pct = 0.75
# train_pct = 0.66
# train_pct = 0.066 # train with 10 instances of 150. Not too bad result!
train_pct = 0.02 # train with 3 instances. Sometimes quite good results.


# Convert Categorical to integer
conv = Dict(
    "setosa" => 1,
    "versicolor" => 2,
    "virginica" => 3,
)
display(describe(iris))
println()


iris2 = Matrix(transform(iris,[:Species] => ByRow(i->conv[i]))[:,[1,2,3,4,6]])
x = iris2[:,1:4]
y = iris2[:,5]

println("\nMean value for each species: ", keys(conv))
display(keys(conv) .|> s -> (s,mean(x[iris.Species.==s,:], dims=1)))
println("\n")

x_train, x_test, y_train, y_test = split_train_test(x,y,train_pct)
num_train_rows = length(y_train)
num_test_rows = length(y_test)

println("Num train instances: $num_train_rows")
println("Num test instances: $num_test_rows")

println("Distribution of the training categories:")
for (k,v) in sort(collect(make_hash(y_train)))
  println("$k : $v ($(100*v/num_train_rows)%)")
end


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

println("\nparameter: credible_intervals")
for v in ["a", "params[1]", "params[2]", "params[3]", "params[4]","sigma"] 
  credible_interval(chns,v,0.95)
end


# display(plot(chns))
println("\nPredictions:")

mpred = linear_regression_model(x_test,missing)
pred = predict(mpred,chns)
display(pred)

cats = Dict(1=>1, 2=>2, 3=>3)
confusion, bads = confusion_matrix(y_test,pred,cats,false)
println("Confusion matrix:")
display(confusion)
# println("\nBad classfied:")
# display(bads)
println("num bad: $(length(bads))") 
#=
println("\nbads: credible_intervals")
for b in bads
  println("b:",b)
  ix = b[1]
  println("x[$ix]: $(x[ix,:])  true cat (y_test[$ix]): $(y_test[ix])  (pred mean: ", mean(pred["y[$ix]"]))
  credible_interval(pred,"y[$(b[1])]",0.3)  
end
=#

println("Distribution of the training categories:")
for (k,v) in sort(collect(make_hash(y_train)))
  println("$k : $v ($(100*v/num_train_rows)%)")
end
