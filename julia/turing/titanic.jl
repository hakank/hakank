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
  


Output 

 Row │ variable  mean      min    median   max    nmissing  eltype   
     │ Symbol    Float64   Int64  Float64  Int64  Int64     DataType 
─────┼───────────────────────────────────────────────────────────────
   1 │ class     2.97728       1      3.0      4         0  Int64
   2 │ age       1.95048       1      2.0      2         0  Int64
   3 │ sex       1.21354       1      1.0      2         0  Int64
   4 │ survived  0.323035      0      0.0      1         0  Int64

Mean value for each categories: [0, 1]
 (0, [3.175838926174497 1.9651006711409396 1.0845637583892618])
 (1, [2.561181434599156 1.919831223628692 1.4838255977496484])

Distribution of the categories
0 : 1490 (67.69650159018627%)
1 : 711 (32.30349840981372%)

mean(x): [7.231329977749316e-16 -1.0330471396784736e-16 1.9369633868971382e-17]
std(x): [0.9999999999999998 0.9999999999999993 0.9999999999999996]

Model:
Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           a    0.3229    0.0088     0.0003    0.0004   1294.2108    1.0011      338.9761
   params[1]   -0.0530    0.0093     0.0003    0.0002   1056.5414    1.0010      276.7264
   params[2]   -0.0208    0.0089     0.0003    0.0002   1789.4310    1.0019      468.6828
   params[3]    0.1908    0.0095     0.0003    0.0003   1073.7710    1.0016      281.2391
       sigma    0.4133    0.0062     0.0002    0.0002   1006.8319    0.9996      263.7066

Predictions:
Num correct: 1708.0
Num incorrect: 493.0
Accuracy: 0.7760109041344844
Confusion matrix:
2×2 Matrix{Float64}:
 1364.0  126.0
  367.0  344.0

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
println("\nMean value for each categories: ", categories)
display(categories .|> s -> (s,mean(x[y.==s,:], dims=1)))
println("\n")

println("Distribution of the categories:")
for (k,v) in make_hash(y)
  println("$k : $v ($(100*v/num_rows)%)")
end

# Standardize the dataset (better and faster)
means = mean(x,dims=1)
stds = std(x,dims=1)
x = (x .- means) ./ stds

println("\nmean(x): ", mean(x,dims=1))
println("std(x): ", std(x,dims=1))
println()

# This is defined in jl_utils.jl
model = linear_regression_model(x,y)

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

mpred = linear_regression_model(x,missing)
pred = predict(mpred,chns)
# display(pred)

cats = Dict(0=>1,1=>2)
confusion, bads = confusion_matrix(y,pred,cats,false)
println("\nConfusion matrix:")
display(confusion)
# println("\nBad classfied:")
# display(bads)
println("\nnum bad: $(length(bads))") 

