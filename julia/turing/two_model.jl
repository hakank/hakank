#=
   Can one not have two models in the same file?
   Yes, it don't seems to be any problem...

=#
using Turing

# Define a simple Normal model with unknown mean and variance.
a = []
@model test_model(n) = begin
        d = TArray{Int}(undef, n)
        # d = Vector{Int}(undef, n)
        for i in 1:n
            d[i] ~ DiscreteUniform(0,9)
        end
        s = sum(d)
        push!(a, s)
end


#  Run sampler, collect results
chn = sample(test_model(10), MH(), 1000)
println("mean(a): $(mean(a))")

a = []
@model test_model2(n) = begin
        d = TArray{Int}(undef, n)
        # d = Vector{Int}(undef, n)
        for i in 1:n
            d[i] ~ DiscreteUniform(0,9)
        end
        s = sum(d)
        push!(a, s)
end


#  Run sampler, collect results
chn = sample(test_model2(10), MH(), 1000)
println("mean(a) 2: $(mean(a))")
