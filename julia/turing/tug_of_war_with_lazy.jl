#=
  Tug of war
  From Hakaru example documentation/tugofwar_rejection.hk

  This variant include laziness

  cf ~/webppl/tug_of_war.wppl
     ~/blog/tug_of_war.blog
=#
using Turing, StatsPlots, DataFrames

@model function tug_of_war(n)

    # strength = Vector{Real}(undef, n)
    # lazy = Vector{Bool}(undef, n)
    # pulls = Vector{Real}(undef, n)
    strength = TArray{Real}(undef, n)
    lazy = TArray{Real}(undef, n)
    pulls = TArray{Real}(undef, n)

    for p in 1:n
        strength[p] ~ truncated(Normal(0,1),0,10) # latent
        lazy[p]     ~ Bernoulli(0.1)
        # Note: This works, but since the pulls[p] is a derived
        #       value, then the pulls data are not in the chns.
        #       So we have to restore this in post process.
        if lazy[p]
            pulls[p] = strength[p] / 2.0
        else
            pulls[p] = strength[p]
        end

    end

    function winner(a,b)
        return pulls[a] > pulls[b] ? a : b
    end

    for p1 in 1:n-1, p2 in p1+1:n
        true ~ Dirac(winner(p1,p2) == p1)
    end
    return pulls
end

num_people = 3
model = tug_of_war(num_people)



num_chns = 4
num_samples = 10_000 # 10_000

# 2.1s Good rhat
chns = sample(model,  MH(), MCMCThreads(), num_samples, num_chns)

# PG(20) doesn't differentiate the people's pull/strength!
# Very slow: 24.6s
# chns = sample(model,  PG(20), MCMCThreads(), num_samples, num_chns)

# IS() doesn't differentiate the people's pull/strength
# chns = sample(model,  IS(), MCMCThreads(), num_samples, num_chns)


# Error
# chns = sample(model,  HMC(0.1, 5), MCMCThreads(), num_samples, num_chns)

# chns = sample(model,  HMCDA(200, 0.65, 0.3), MCMCThreads(), num_samples, num_chns)
# chns = sample(model,  NUTS(1000, 0.65), MCMCThreads(), num_samples, num_chns)

# Too slow
# chns = sample(model,  SMC(), MCMCThreads(), num_samples, num_chns)

# chns = sample(model,  NUTS(1000,0.65), MCMCThreads(), num_samples, num_chns) # Error
# Not correct: lazy is all 0 and neither strength nor pulls are "ordered"
# (and lots of rejected proposals)
# chns = sample(model,  Gibbs(NUTS(1000,0.65,:pulls,:strength),MH(:lazy)), MCMCThreads(), num_samples, num_chns)


# chns = sample(model,  HMC(0.1, 5), MCMCThreads(), num_samples, num_chns) # error
# This works: 6.6s. Good rhats.
# chns = sample(model,  Gibbs(HMC(0.1,5,:pulls,:strength),MH(:lazy)), MCMCThreads(), num_samples, num_chns)


display(chns)
# display(gelmandiag(chns))
# display(plot(chns))
df = DataFrame(chns)


# Show the latent strength
println("strength (latent):")
for p1 in 1:num_people-1, p2 in p1+1:num_people
    println("strength[$p1] > strength[$p2]: ", mean(df[!,"strength[$p1]"] .>df[!,"strength[$p2]"]))
end

if "pulls[1]" in names(df)
    # This is for the now commented version of pulls.
    # But that don't work well.
    println("pulls (observed):")
    for p1 in 1:num_people-1, p2 in p1+1:num_people
        println("pulls[$p1] > pulls[$p2]: ", mean(df[!,"pulls[$p1]"] .>df[!,"pulls[$p2]"]))
    end
else
   println("Restoring pulls")
   #
   # Since the pulls are not available from the chns we have to restore it
   # (Note: This is one of the disadvantages that Turing has compared to e.g. WebPPL!)
   println("\npulls")
   pulls = []
   for row in eachrow(df)
       r = vcat([ row["lazy[$p]"] == 1.0 ? row["strength[$p]"] / 2 : row["strength[$p]"]  for p in 1:num_people]...)
       push!(pulls, r)
   end

   println("\npulls (restored):")
   println("pulls overall mean: ", mean(pulls))

   println("\ncomparison of pulls")
   for p1 in 1:num_people-1, p2 in p1+1:num_people
       # println("pulls[$p1] > pulls[$p2]: ", mean(pulls[:,p1] .> pulls[:,p2]))
       println("pulls[$p1] > pulls[$p2]: ", mean([p[p1] > p[p2] for p in pulls]))
   end

end
