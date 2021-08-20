#=

    This is a port of the R2 model DigitRecognition.cs

    with the datafiles 
    - digit_recognotion_input.csv  28 x 28
    - digit_recognotion_params.csv 28 x 28 x 10
    - digit_recognotion_prior.csv  for 0..9

    The image (that would be a 3):
        0000000000000000000000000000
        0000000000001111100000000000
        0000000111111111111000000000
        0000001111111111111000000000
        0000011111111111111000000000
        0000001111111111111000000000
        0000001110000001111000000000
        0000000000000001111000000000
        0000000000000011111000000000
        0000000000000111111000000000
        0000000000001111110000000000
        0000000000011111110000000000
        0000000000111111111000000000
        0000000000111111111100000000
        0000000000011111111110000000
        0000000000000000111110000000
        0000000000000000011110000000
        0000000000000000011110000000
        0000000000000000111110000000
        0000000000000011111110000000
        0011000000001111111100000000
        0111111111111111111000000000
        0111111111111111110000000000
        0111111111111111000000000000
        0011111111111100000000000000
        0000000000000000000000000000
        0000000000000000000000000000
        0000000000000000000000000000

    The R2 model outputs:
    ```
    Mean: 3.021
    Variance: 0.0405996
    Number of accepted samples = 89
    ```


    This Turing.jl model, were y0 is the digits (0..9) (y is 1..10)

    Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

            y    3.9970    0.0949     0.0030    0.0030   1000.0080    1.0000       59.9238
            y0    2.9970    0.0949     0.0030    0.0030   1000.0080    1.0000       59.9238


    Distributions of variable y (num:0)
    4.00000 =>     999  (0.999000)
    1.00000 =>       1  (0.001000)
    Distributions of variable y0 (num:0)
    3.00000 =>     999  (0.999000)
    0.00000 =>       1  (0.001000)
   

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function digit_recognition(x,params,prior) 
    x_len = length(x)

    y ~ Categorical(simplex(prior)) # this is 1..10
    # A uniform prior give the same result
    # y ~ Categorical(simplex([1/10 for i in 1:10]))
    y0 ~ Dirac(y-1) # adjust to digits 0..9
    for i in 1:x_len
        x[i] ~ Bernoulli(params[y,i])
    end
end 

input = parse.(Int,split(readline("digit_recognition_input.csv"),","))
input_img = reshape(input,28,28)
for i in 1:28
    println(join(string.(input_img)[i,:]))
end
println()

params = Matrix(CSV.read("digit_recognition_params.csv",header=false,DataFrame))
prior = parse.(Float64,split(readline("digit_recognition_prior.csv"),","))

println("prior:$prior len:$(length(prior)) sum(prior):$(sum(prior))")


model = digit_recognition(input,params,prior)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)

show_var_dist_pct(chns,:y)
show_var_dist_pct(chns,:y0)