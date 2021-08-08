#=
    Badges problem

    See my analysis of the Badges problem written:
    http://www.hakank.org/data_mining/badge_problem.html
    """
    One of the best collections of machine learning data sets includes this "recreational" data set called "Badge Problem". I haven't seen any solutions to the problem anywhere, 
    so here is my take.
    

    [This info is from the datafile:]

    1. Title: ML94/COLT94 Badge Problem

    2. Source Information
    -- Creator: Haym Hirsh, after an idea by Rob Schapire
    -- Donor: Haym Hirsh (hirsh@cs.rutgers.edu)
    -- Date: September, 1994
    
    3. Past Usage:
        Every pre-registered attendee at the 1994 Machine Learning
        Conference and 1994 Computational Learning Theory Conference received
        a badge labeled with a "+" or "-".  The labeling was due to some
        function known only to the badge generator (Haym Hirsh), and it
        depended only on the attendee's name.  The goal for conference
        attendees was to identify the unknown function used to generate the
        +/- labeling.

    4. Relevant Information:
        Part of the problem in using an automated program to discover the
        unknown target function is to decide how to encode names such that
        the program can be used.  The data below are presented in the form
        of a +/- label followed by the person's name.  It is up to the
        learning-system user to decide how to convert this data into something
        usable by the system (e.g., what attributes to use if your favorite
        learner requires feature-vector data).

    """

    Note: The ARFF file http://www.hakank.org/data_mining/badges_plain.arff
    includes all the preprocessed dataset, but it's more fun to do everything
    in Julia. 
    I have saved the converted CSV file to badges.csv for easier handling.

    Here are the (derived) attributes:
        even_odd         {0,1} 	length of name even?
        first_char_vowel {0,1} 	is first character a vowel?
        second_char_vowel {0,1} 	is second character a vowel?
        vowels numeric 	number of vowels in the name
        consonants numeric 	number of consonants
        vowel_consonant_ratio numeric 	the ratio of vowels / consonant (this is not farfetched???)
        spaces numeric 	number of spaces
        dots: number of "." in the name, i.e. name initials
        words: number of words, i.e number of names, including name initials..

        class {1,0} 	the badge labels

    The best attribute is the params[3] (second_char_vowel) which happens to be correct.

    Compared to badges.jl this version use a different training and test set.

    Note: by guessing 1 all the time, we get an accuracy of 71%.

    Some results: A training percentage below about 20% we start to get quite bad 
    results, i.e. <= 71%.

    * train pct 0.66

    Num correct: 100.0
    Num incorrect: 0.0
    Accuracy: 1.0
    Confusion matrix:
    2×2 Matrix{Float64}:
    31.0   0.0
    0.0  69.0
    num bad: 0

    Distribution of the categories (training set):
    0.0 : 53 (27.46113989637306%)
    1.0 : 140 (72.53886010362694%)

    * train_pct 0.33
    Num correct: 186.0
    Num incorrect: 10.0
    Accuracy: 0.9489795918367347
    Confusion matrix:
    2×2 Matrix{Float64}:
    49.0   10.0
    0.0  137.0
    num bad: 10

    Distribution of the categories (training set):
    0.0 : 25 (25.77319587628866%)
    1.0 : 72 (74.22680412371135%)

    * traininc_pct 0.10
    Num correct: 195.0
    Num incorrect: 69.0
    Accuracy: 0.7386363636363636
    Confusion matrix:
    2×2 Matrix{Float64}:
     9.0   69.0
     0.0  186.0
    num bad: 69

    Distribution of the categories (training set):
    0.0 : 6 (20.689655172413794%)
    1.0 : 23 (79.3103448275862%)

=#
using Turing, StatsPlots, Distributions
using LinearAlgebra
using DataFrames
using HTTP, CSV
# pyplot()
include("jl_utils.jl")

#=
# Read the dataset (from my site)
# Saved as a CSV file for easier handling in this program.
badges_url = "http://www.hakank.org/data_mining/badges.data"
dataset = CSV.read(Download.download(badges_url))
CSV.write("badges.csv",dataset)
=#


function convert_name(str)
  xcat,name = String.(split(str," ",limit=2))
  len = length(name)
  even_len = len % 2 == 0 ? 1 : 0 # Is length even?
  first_char_vowel = name[1] in vowels ? 1 : 0 # is first char in name a vowel?
  second_char_vowel = name[2] in vowels ? 1 : 0 # is second char in name a vowel?
  num_vowels = filter(c -> c in vowels, name) |> length # number of vowels
  num_consonants = filter(c -> !(c in vowels), name) |> length # number of consonants
  vowel_consonant_ratio = num_vowels / num_consonants > 0.5 ? 1 : 0  # vowels / consonants
  num_spaces = filter(c -> c == " ", name) |> length # number of spaces
  num_dots = filter(c -> c == ".", name) |> length # number of dots
  num_words = split(name, " ") |> length 
          #    1        2                3                 4          5              6                     7          8        9
  return xcat, [even_len first_char_vowel second_char_vowel num_vowels num_consonants vowel_consonant_ratio num_spaces num_dots num_words]

end

@model function badges_model(x,y=missing, ::Type{T} = Float64) where {T}
    n,p = size(x)  
    if y === missing 
      y = Vector{T}(undef, n) 
    end

    a ~ Uniform(0,1)
    params ~ filldist(Uniform(0,1),p)  
    sigma ~ Uniform(0,1)

    ## Very slow, and give bad rhats
    # for i in 1:n
    #     mu = a + dot(x[i,:], params)
    #     # mu = a .+ x[i,:]' * params
    #     # y[i] ~ Normal(mu,sigma) # very slowe, bad rhat 
    #     y[i] ~ Logistic(mu,sigma) # very slow, nice rhat 
    # end

    # Better, much faster and neater
    mu = a .+ x*params
    y ~ MvNormal(mu, 1)
    
    # Return the classes for prediction.
    return y

end


df = CSV.read("badges.csv",DataFrame)
vowels = "aeiou" # note: y is not a vowel
# println(data)
x = []
y = []
for d in eachrow(df)
    cat, v = convert_name(d[1])
    push!(x,v)
    if cat == "+"
        push!(y,1)
    else
        push!(y,0)
    end
end
y = 1.0*y
x = 1.0 .* x
x = vcat(x...)
num_rows, num_cols = size(x)
println("num_rows: $num_rows num_cols: $num_cols")

println("\nDistribution of the categories (full dataset):")
for (k,v) in make_hash(y)
  println("$k : $v ($(100*v/num_rows)%)")
end


println("\n3 random instances of x:")
display(x[rand(1:size(x)[1],3),:])


# Training percentage 
# train_pct = 0.99
# train_pct = 0.75
train_pct = 0.66
# train_pct = 0.33
# train_pct = 0.20
# train_pct = 0.15
# train_pct = 0.10
# train_pct = 0.05 # 15 instances
# train_pct = 10/num_rows
# train_pct = 2/num_rows

x_train, x_test, y_train, y_test = split_train_test(x,y,train_pct)

num_train_rows = length(y_train)
num_test_rows = length(y_test)

println("Num training instances: $num_train_rows")
println("Num test instances: $num_test_rows")

conv = Dict(0=>1,1=>2)
println("\nMean value for each categories (training set): ", keys(conv))
display(keys(conv) .|> s -> (s,mean(y_train .==s, dims=1)))
println("\n")

println("\nDistribution of the categories (training set):")
for (k,v) in make_hash(y_train)
  println("$k : $v ($(100*v/num_train_rows)%)")
end


#
# Train the model  
#

model = badges_model(x_train,y_train)

println("\nModel:")
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, HMC(0.1,6), 1_000)
chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

println("\nPredictions:")

mpred = badges_model(x_test,missing)
pred = predict(mpred,chns)
display(pred)
println()

cats = Dict(0=>1,1=>2)
confusion, bads = confusion_matrix(y_test,pred,cats,false)
println("Confusion matrix:")
display(confusion)
# println("\nBad classfied:")
# display(bads)
println("num bad: $(length(bads))") 

println("\nDistribution of the categories (training set):")
for (k,v) in make_hash(y_train)
  println("$k : $v ($(100*v/num_train_rows)%)")
end

