#=
  Utilities for Turing.jl

  Here are utils for my Turing.jl programming.

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Turing.jl page: http://www.hakank.org/julia/turing/

=#

using Turing
using Printf, Distributions
using LinearAlgebra

"""
Return a Dict of the elements and their occurences
in the collection a.
"""
function make_hash(a)
    d = Dict()
    for e in collect(a)
        # get!(d,e,0)
        if !haskey(d,e)
            d[e] = 0
        end
        d[e] += 1
    end
    return d
end

"""
This is to simplify the conversion from WebPPL
"""
function flip(p=0.5)
    return Bernoulli(p)
end


"""
Return a normalized vector, i.e. where the sum is 1
"""
function simplex(v)
    return v./sum(v)
end

"""
Show a sorted Dict with keys, values and percentages
Example
julia> show_var_dist(chns,:d1)

Distributions of variable d1
 1 =>   13654  0.341350
 2 =>   13417  0.335425
 3 =>   12929  0.323225

Note: I'm not sure how to get more than one variables
E.g. this don't work now:
 julia> show_var_dist(chns,[:d1,:d2])
"""
function show_var_dist(chns, var)

    if var in chns.name_map.parameters
        println("Distributions of variable $var")
        len = length(vcat(chns[var]...)) # handle multiple chains
        for kv in sort(make_hash(chns[var]))
            @printf "%-3.5f => % 7d  %2.6f\n" kv[1] kv[2] kv[2]/len
        end
    else
        # println("Variable $var is not in chains")
    end
end

"""
Show distribution of a variable in a MCMCChain
Sort the dictionary in order of decreasing occurrence (percentage)
Examples:

 - show_var_dist_pct(chns, :n)       show all entries

 - show_var_dist_pct(chns, :n, 10)  show first 10 entries (e.g. for large tables)
"""
function show_var_dist_pct(chns::Chains, var, num=0)

    if var in chns.name_map.parameters
        println("Distributions of variable $var (num:$num)")
        len = length(vcat(chns[var]...)) # handle multiple chains
        c = 0
        for kv in sort(collect(make_hash(chns[var])),by=x->x[2],rev=true)
            c += 1
            if (num == 0) || (num > 0 && c <= num)
                @printf "%-3.5f => % 7d  (%2.6f)\n" kv[1] kv[2] kv[2]/len
            end
        end
    else
        # println("Variable $var is not in chains")
    end
end


"""
Show distribution of a variable in a MCMCChain

Sort the dictionary in order of decreasing occurrence (percentage)
and present the values from array `a` where the position in 
`a` represents the values `1..length(a)`.

Examples:

 - show_var_dist_pct(chns, :x, ["yellow","blue", "green"])


"""
function show_var_dist_pct(chns::Chains, var, a::Array)

    if var in chns.name_map.parameters
        println("Distributions of variable $var")
        len = length(vcat(chns[var]...)) # handle multiple chains
        for kv in sort(collect(make_hash(chns[var])),by=x->x[2],rev=true)
            ix = round(Int,kv[1])
            @printf "%-10s => % 7d  (%2.6f)\n" a[ix] kv[2] kv[2]/len
        end
    else
        # println("Variable $var is not in chains")
    end
end


"""
Show distribution of the elements in a (simple) Array.
Sort the dictionary in order of decreasing occurrence (percentage)
Examples:

 - show_var_dist_pct(a)       show all entries

 - show_var_dist_pct(a, 10)  show first 10 entries (e.g. for large tables)

Full example:

julia> show_var_dist_pct(rand(Binomial(100,0.1),100000),10)

 Distributions of variable (num:10)
 10.00000 =>   13143  0.131430
 9.00000 =>   13094  0.130940
 11.00000 =>   12002  0.120020
 8.00000 =>   11403  0.114030
 12.00000 =>    9989  0.099890
 7.00000 =>    8765  0.087650
 13.00000 =>    7475  0.074750
 6.00000 =>    5968  0.059680
 14.00000 =>    5117  0.051170
 5.00000 =>    3450  0.034500
"""
function show_var_dist_pct(a::Array, num=0)
    println("Distributions of variable (num:$num)")
    c = 0
    len = length(a)
    isArray = false
    if length(a[1]) > 1 # typeof(a[1]) <: Array
        isArray = true
    end
    for kv in sort(collect(make_hash(a)),by=x->x[2],rev=true)
        c += 1
        if (num == 0) || (num > 0 && c <= num)
            if isArray
                # This is an array, vector, tuple of elements
                # len = length(kv[1])
                # format = "%-$(len)s => % 7d  %2.6f\n"
                # @printf format kv[1] kv[2] kv[2]/len
                # @printf "%-35s => % 7d  %2.6f\n" kv[1] kv[2] kv[2]/len
                println("$(kv[1])\t=>\t$(kv[2]) ($(kv[2]/len))")
            else
                @printf "%-3.5f => % 7d  (%2.6f)\n" kv[1] kv[2] kv[2]/len
            end
        end
    end
end

"""
This is ported from the WebPPL function in
https://mhtess.github.io/bdappl/chapters/03-simpleModels.html

Example in credible_interval_test.jl:

julia> credible_interval(chns, "posteriorPredictive",0.90)

  credible interval for posteriorPredictive with mass 0.9: (10.000000 .. 18.000000)
"""
function credible_interval(chns::Chains, var, credMass=0.95)
    # Using sort and vcat don't work, and this reshaping is clunky. TODO!
    sss = prod(size(chns[var]))
    chnsCat = reshape(chns[var],sss,1)
    sortedPts = sort(chnsCat,dims=1)
    len = length(sortedPts)
    ciIdxInc = round(Int,ceil(credMass*len))
    nCIs = len - ciIdxInc
    ciWidth = map(i->sortedPts[i+ciIdxInc]-sortedPts[i], 1:nCIs)
    _, i = findmin(ciWidth)
    print("Credible interval for $(var) with mass $(credMass): ")    
    if typeof(sortedPts[1]) == Int64
        @printf("(%d..%d)\n", sortedPts[i],sortedPts[i+ciIdxInc])
    else
        @printf("(%f..%f)\n", sortedPts[i],sortedPts[i+ciIdxInc])
    end

end

"""
Return mean value of the variable var in the chain
"""
function mean_val(chns::Chains, var)
    mean(chns[var])
end

"""
`UniformDraw(xs)`

Draw one element from `xs` randomly with uniform probability.

For a version with handcrafted probability, see `DiscreteNonParametric`.

"""
UniformDraw(xs) = DiscreteNonParametric(xs, ones(length(xs)) ./ length(xs))


"""
`confusion_matrix(y,pred,cats,print_all=true)``

Given the true classifications `y` and the predictions `pred`,
it returns a confusion matrix as well as a bads array for the wrongly 
predicted instances (in the for id=>(true_cat,pred_cat)).

Also prints info about all the instances if `print_all=true`.

`cats` is a dictionary converting the numeric category to a 
number 1:number_of categories; this is for handling the 
confusion matrix. (Perhaps there's a better way of doing this...)
"""
function confusion_matrix(y,pred,cats,print_all=true)
    n = length(y)
    bads = []
    num_cat = cats  |> length
    confusion = zeros(num_cat,num_cat)
    min_val, max_val = extrema(keys(cats))
    for i in 1:n
      true_cat = round(Int64,y[i])
      p = mean(pred["y[$i]"])
      diff = y[i]-p
      pred_cat = round(Int64,p)
      # println("true_cat: $true_cat pred_cat: $pred_cat")
      # Handle cases were we got predicates out of range
      # i.e. 0 when the categories is 1..
      if pred_cat < min_val 
        println("pred_cat < min_val. Adjusting to min_val")
        pred_cat = min_val 
      end
      if pred_cat > max_val 
        println("pred_cat > max_val. Adjusting to max_val")
        pred_cat = max_val 
      end
      abs_diff = abs(diff)
      
      if pred_cat != true_cat
        bad = "BAD!"
        # the correct cat is predicted (wrongly) as pred_cat
        push!(bads, i=>(true_cat=>pred_cat))
        confusion[cats[true_cat],cats[pred_cat]] += 1
      else
        bad = ""
        confusion[cats[true_cat],cats[true_cat]] += 1    
      end
      if print_all 
        println("y[$i]: true cat: $true_cat pred[$i]: ", p, " diff: ", y[i]-p, " pred cat: $pred_cat $bad")
      end
    end

    num_correct = sum(diag(confusion))
    num_total = sum(confusion)
    println("Num correct: $num_correct")
    println("Num incorrect: ", num_total - num_correct)
    println("Accuracy: ",  (num_correct / num_total))
    return confusion, bads
end  


"""
`linear_regression_model(x,y=missing)`

BDA model for linear regression using MvNormal and rather strict priors.

`x` is the dataframe with the input features, `y` is the output variable (a single value).

The model can be used in prediction with the following workflow:
```
# Training: x and y is the training data  
model = linear_regression_model(x,y)
chns = sample(model, NUTS(), 1_000)
display(chns)

# Prediction, note the "missing" argument instead if y
mpred = linear_regression_model(x,missing)
pred = predict(mpred,chns)
display(pred)`
```   

"""
@model function linear_regression_model(x,y=missing, ::Type{T} = Float64) where {T}
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


"""
`split_train_test(x,y,train_pct=0.75)`

Split the datasets `x` (the inputs) and `y` (the outputs) into 
training and test sets with a size of the training set of `train_pct`.

Returns `x_train, x_test, y_train, y_test`
"""
function split_train_test(x,y,train_pct=0.75)
    num_rows = length(y)
    num_train = round(Int64,num_rows*train_pct)

    train_ix = StatsBase.sample(1:num_rows,num_train,replace=false)
    test_ix = [i for i in 1:num_rows if !(i in train_ix)]

    # Split into train and test
    x_train = x[train_ix,:]
    x_test = x[test_ix,:]


    y_train = y[train_ix]
    y_test = y[test_ix]


    x_train, x_test, y_train, y_test
end