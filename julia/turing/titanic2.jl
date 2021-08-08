#=
    Bayesian Data Analysis of Titanic data set 

    This is also a test of the ARFFFiles package for reading ARFF Files
    (the format used in the data mining system Weka, which happens to 
    be one of my favorite data mining system).
        
    The file used is from my Weka page: http://www.hakank.org/weka/titanic.arff

    This is the simple version of the Titanic dataset with 2201 rows and the following
    attributes, their values (and the transformed values):
    - class: 1st, 2nd, 3rd, crew (=> 1, 2, 3, 4)
    - age  : adult, child        (=> 1, 2)
    - sex  : male, female        (=> 1, 2)
    - survived: yes, no          (=> 1, 0)
  
    Compared to titanic.jl, this program tests the model with a different train and 
    test sets. The accuracy for the titanic.jl model is about 77.6% (using the full dataset).

    Here are some results with different training percentages.

    * train pct 0.99
    Num correct: 19.0
    Num incorrect: 3.0
    Accuracy: 0.8636363636363636

    Confusion matrix:
    2×2 Matrix{Float64}:
    17.0  0.0
     3.0  2.0

    num bad: 3
    Distribution of the training categories:
    0 : 1473 (67.59981642955483%)
    1 : 706 (32.40018357044516%)
   

    * train_pct 0.75
    Num correct: 417.0
    Num incorrect: 133.0
    Accuracy: 0.7581818181818182

    Confusion matrix:
    2×2 Matrix{Float64}:
    328.0  39.0
     94.0  89.0

    num bad: 133
    Distribution of the training categories:
    0 : 1123 (68.01938219261054%)
    1 : 528 (31.98061780738946%)


    * train pct 0.66
    Num correct: 586.0
    Num incorrect: 162.0
    Accuracy: 0.7834224598930482

    Confusion matrix:
    2×2 Matrix{Float64}:
    461.0   50.0
    112.0  125.0

    num bad: 162
    Distribution of the training categories:
    0 : 979 (67.37783895388851%)
    1 : 474 (32.62216104611149%)

    * train pct 0.33
    Num correct: 1128.0
    Num incorrect: 347.0
    Accuracy: 0.7647457627118645

    Confusion matrix:
    2×2 Matrix{Float64}:
    908.0   89.0
    258.0  220.0

    num bad: 347
    Distribution of the training categories:
    0 : 493 (67.90633608815428%)
    1 : 233 (32.09366391184573%)


    * train_pct 0.1 
    Num correct: 1525.0
    Num incorrect: 456.0
    Accuracy: 0.7698132256436143

    Confusion matrix:
    2×2 Matrix{Float64}:
    1209.0  114.0
     342.0  316.0

    num bad: 456
    Distribution of the training categories:
    0 : 167 (75.9090909090909%)
    1 : 53 (24.09090909090909%)


    * train pct 0.004543389368468878 (10 training instances (of 2201)
    Num correct: 1679.0
    Num incorrect: 512.0
    Accuracy: 0.7663167503423094

    Confusion matrix:
    2×2 Matrix{Float64}:
    1370.0  114.0
     398.0  309.0

    num bad: 512
    Distribution of the training categories:
    0 : 6 (60.0%)
    1 : 4 (40.0%)


    * train pct 0.0009086778736937755 (2 training instances of 2201), no standardizing
    This is an extreme case with just 2 training instances (but I had to try this).
    Note that there are 67% change of just guessing the correct value, by always
    selecting the majority category. 

    When there are two different categories there's often/sometimes a better chance (~ 76%)
    of getting the correct value, almost as good as with the full dataset...

    Num correct: 1632.0
    Num incorrect: 567.0
    Accuracy: 0.7421555252387448

    Confusion matrix:
    2×2 Matrix{Float64}:
    1274.0  215.0
     352.0  358.0

    num bad: 567
    Distribution of the training categories:
    0 : 1 (50.0%)
    1 : 1 (50.0%)

    But sometimes this fails. Here we have one training instance of each category, but 
    got an accuracy 30%, which is worse than just guessing the majority category: 

    Num correct: 677.0
    Num incorrect: 1522.0
    Accuracy: 0.30786721236925874

    Confusion matrix:
    2×2 Matrix{Float64}:
    398.0  1091.0
    431.0   279.0

    num bad: 1522
    Distribution of the training categories:
    0 : 1 (50.0%)
    1 : 1 (50.0%)


    Here is an example when the two training instance are from the same category:

    Num correct: 1488.0
    Num incorrect: 711.0
    Accuracy: 0.6766712141882674

    Confusion matrix:
    2×2 Matrix{Float64}:
    1488.0  0.0
     711.0  0.0

    num bad: 711
    Distribution of the training categories:
    0 : 2 (100.0%)

   Another example (with <30% accuracy)
    Num correct: 636.0
    Num incorrect: 1563.0
    Accuracy: 0.2892223738062756

    Confusion matrix:
    2×2 Matrix{Float64}:
     65.0  1425.0
    138.0   571.0

    num bad: 1563
    Distribution of the training categories:
    1 : 2 (100.0%)
  

=#
using Turing, StatsPlots, Distributions
using DataFrames, StatsBase
using CSV
using ARFFFiles
include("jl_utils.jl")

#
# Read dataset and preprocessing
#
df = DataFrame(ARFFFiles.load(download("http://www.hakank.org/weka/titanic.arff")))
display(describe(df))
println()

# Convert categories -> numeric values
transform!(df, [:class]    => ByRow(i -> Dict("1st"=>1,"2nd"=>2,"3rd"=>3,"crew"=>4)[i]) => :class)
transform!(df, [:age]      => ByRow(i -> Dict("child"=>1,"adult"=>2)[i]) => :age)
transform!(df, [:sex]      => ByRow(i -> Dict("male"=>1,"female"=>2)[i]) => :sex)
transform!(df, [:survived] => ByRow(i -> Dict("no"=>0,"yes"=>1)[i]) => :survived)

display(describe(df))
println()

# Convert to matrix and extract x and y.
num_rows,num_cols = size(df)
println("Num rows: $num_rows Num columns: $num_cols")
data = Matrix(df)
x = data[:,1:end-1]
y = data[:,end]

categories = unique(y) |> sort 
println("\nMean value for each categories (full dataset): ", categories)
display(categories .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")

println("\nDistribution of the categories (full dataset):")
for (k,v) in make_hash(y)
  println("$k : $v ($(100*v/num_rows)%)")
end


# train_pct = 0.99
train_pct = 0.75
# train_pct = 0.66
# train_pct = 0.33
# train_pct = 0.1
# train_pct = 10/num_rows # 10 instances
# train_pct = 4/num_rows # 4 instances
# train_pct = 2/num_rows # 2 instances

println("\nTrain percentage: $train_pct")

x_train, x_test, y_train, y_test = split_train_test(x,y,train_pct)
num_train_rows = length(y_train)
num_test_rows = length(y_test)

println("Num training instances: $num_train_rows")
println("Num training instances: $num_test_rows")


println("Distribution of the training categories:")
for (k,v) in make_hash(y_train)
  println("$k : $v ($(100*v/num_train_rows)%)")
end

# Standardize the dataset (better and faster)
# Separate train and test set.
# if train_pct >= 0.2
#     means_train = mean(x_train,dims=1)
#     stds_train = std(x_train,dims=1)
#     x_train = (x_train .- means_train) ./ stds_train
#
#     means_test = mean(x_test,dims=1)
#     stds_test = std(x_test,dims=1)
#     x_train = (x_train .- means_test) ./ stds_test
# end

println("\nmean(x_train): ", mean(x_train,dims=1))
println("std(x_train): ", std(x_train,dims=1))
println()

# This is defined in jl_utils.jl
model = linear_regression_model(x_train,y_train)

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

mpred = linear_regression_model(x_test,missing)
pred = predict(mpred,chns)
# display(pred)

cats = Dict(0=>1,1=>2)
confusion, bads = confusion_matrix(y_test,pred,cats,false)
println("\nConfusion matrix:")
display(confusion)
# println("\nBad classfied:")
# display(bads)
println("\nnum bad: $(length(bads))") 


println("Distribution of the training categories:")
for (k,v) in make_hash(y_train)
  println("$k : $v ($(100*v/num_train_rows)%)")
end
