#=
  From BLOG Swift example birthday.blog
  https://github.com/lileicc/swift/blob/master/example/birthday.blog
  Result
  """
  Distribution of values for exists Person x (exists Person y ((!(x = y) & (Birthday(x) = Birthday(y)))))
	true	0.5446927374301685
	false	0.4553072625698328
  """

=#
using Turing, StatsPlots, DataFrames, MCMCChains
include("jl_utils.jl")


@model function birthday(num_people=missing)
    # var day = _.range(365);
    num_days = 365

    if num_people === missing
        num_people ~ truncated(Poisson(20),1,num_days)
    end

    # Note: I call it zbirthdays so it's displayed last
    zbirthdays = Vector{Int}(undef, num_people)
    for d in 1:num_people
       zbirthdays[d] ~ DiscreteUniform(1,num_days)
    end


    #  is there a duplicate of birthdays?
    # hakank: I'm not sure if the is the best (or even good) way
    #         to model this...
    b = make_hash(zbirthdays) # Collect the birthdays
    s = sum(values(b) .> 1)
    if s == 0
        t ~ Bernoulli(0)
    else
        t ~ Bernoulli(1)
    end

end


model = birthday(23)
# model = birthday()

num_chns = 4
println("Prior distribution")
chns_prior = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
display(chns_prior)
# display(plot(chns_prior))

println("\nPosterior distribution")
num_chns = 4

# chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, MH(), MCMCThreads(), 30_000, num_chns)

chns = sample(model, IS(), MCMCThreads(), 10_000, num_chns) # ORIG

# chns = sample(model, PG(20), MCMCThreads(), 1000, num_chns)

# chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)



## LoadError: MethodError: Cannot `convert` an object of type Float64 to an object of type Symbol
# chns = sample(model, Gibbs(NUTS(0.65,5,1,1,0,:lambda_1,:lambda_2), PG(20,:tau) ), 1000)
# chns = sample(model, Gibbs(HMC(0.1,5,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chns)

# Error
# chns = sample(model, Gibbs(HMCDA(0.15,0.65,0.3,:lambda_1,:lambda_2), PG(20,:tau) ), MCMCThreads(), 1000, num_chns) # Error...

# Skipping this since it contains a lot of uninteresting zbirthdays[b]
# display(chns)
println("t:")
display(chns[:t])

# plot(chns)

if :num_people in chns.name_map.parameters
    println("num_people:")
    display(chns[:num_people])
end

# df = DataFrame(chns)
# display(df)
