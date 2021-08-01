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

   Here's the output of the model:

Summary Statistics
  parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           a    0.0452    0.0407     0.0013    0.0015   1052.1756    0.9990      227.9410
   params[1]    0.0580    0.0463     0.0015    0.0011    779.7220    0.9995      168.9172
   params[2]    0.4932    0.2944     0.0093    0.0082   1112.2659    0.9995      240.9588
   params[3]    0.7670    0.1014     0.0032    0.0046    602.2420    1.0003      130.4684
   params[4]    0.0088    0.0082     0.0003    0.0004    591.0964    1.0011      128.0538
   params[5]    0.0045    0.0040     0.0001    0.0002    863.8360    1.0053      187.1395
   params[6]    0.0808    0.0623     0.0020    0.0019    814.4369    1.0009      176.4378
   params[7]    0.4963    0.2794     0.0088    0.0049   1090.7765    0.9991      236.3034
   params[8]    0.4994    0.3009     0.0095    0.0134    891.6018    0.9999      193.1546
   params[9]    0.0207    0.0186     0.0006    0.0004    930.5087    0.9999      201.5833
       sigma    0.4964    0.2985     0.0094    0.0088    796.7233    0.9991      172.6004

...

Predictions:

Confusion matrix:
2×2 Matrix{Float64}:
 84.0    0.0
  0.0  209.0

Bad classfied:
Any[]
num bad: 0


Testing: "+ Hakan Kjellerstrand" give this (correct) prediction, i.e. that it's a "+":


Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64 

        y[1]    1.0067    0.9834     0.0311    0.0363   793.7538    1.0008


As we can see, the model does a pretty nice work identifying the proper class.

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
println("size(x): $(size(x))")

println("3 random instances of x:")
display(x[rand(1:size(x)[1],3),:])

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

model = badges_model(x,y)


# println("Prior:")
# chns = sample(model, Prior(), 10_000)
# display(chns)

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

mpred = badges_model(x,missing)
pred = predict(mpred,chns)
display(pred)

cats = Dict(0=>1,1=>2)
confusion, bads = confusion_matrix(y,pred,cats,true)
println("Confusion matrix:")
display(confusion)
println("\nBad classfied:")
display(bads)
println("num bad: $(length(bads))") 
if length(bads) > 0 
  println("\nbads: credible_intervals")
  for b in bads
    println("b:",b)
    ix = b[1]
    println("x[$ix]: $(x[ix,:])  true cat (y[$ix]): $(y[ix])  (pred mean: ", mean(pred["y[$ix]"]))
    credible_interval(pred,"y[$(b[1])]",0.3)  
  end
end

println("Testing an unknown name (I was not at the ML94/COLT94 conference)")
xcat, x = convert_name("+ Hakan Kjellerstrand")
println("xcat:$xcat x:$x")
mpred = badges_model(x,missing)
pred = predict(mpred,chns)
display(pred)
println("prediction: ", round(Int64,mean(pred["y[1]"])))