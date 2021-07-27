#=
  Tug of war
   From Hakaru example documentation/tugofwar_rejection.hk

  Compare with true_skill.jl

  cf ~/webppl/tug_of_war.wppl
     ~/blog/tug_of_war.blog
=#
using Turing, StatsPlots, DataFrames

#=
# For num_people = 5
strength (latent):
strength[1] > strength[2]: 0.67203
strength[1] > strength[3]: 0.641555
strength[1] > strength[4]: 0.76379
strength[1] > strength[5]: 0.73293
strength[2] > strength[3]: 0.60609
strength[2] > strength[4]: 0.69859
strength[2] > strength[5]: 0.688345
strength[3] > strength[4]: 0.54991
strength[3] > strength[5]: 0.640705
strength[4] > strength[5]: 0.49402
pulls (observed):
pulls[1] > pulls[2]: 1.0
pulls[1] > pulls[3]: 1.0
pulls[1] > pulls[4]: 1.0
pulls[1] > pulls[5]: 1.0
pulls[2] > pulls[3]: 1.0
pulls[2] > pulls[4]: 1.0
pulls[2] > pulls[5]: 1.0
pulls[3] > pulls[4]: 0.99546
pulls[3] > pulls[5]: 0.99546
pulls[4] > pulls[5]: 1.0

=#
@model function tug_of_war(n)

    strength = TArray{Real}(undef, n)
    pulls = TArray{Real}(undef, n)
    # strength = Vector{Real}(undef, n)
    # pulls = Vector{Real}(undef, n)

    for p in 1:n
        strength[p] ~ Uniform(0,1) # truncated(Normal(0,1),0,10) # latent
        pulls[p]    ~ truncated(Normal(strength[p],1),0,10) # observed # truncated(Normal(strength[p],1),0,10) # observed
    end

    # The Hakary/BLOG model had "pulls(a) < pulls(b) "
    # which is a little strange so I change to return the winner.
    function winner(a,b)
        return pulls[a] > pulls[b] ? a : b
    end

    # match1 = winner(alice, bob)
    # match2 = winner(bob, carol)
    # match3 = winner(alice, carol)

    # match1 = winner(1, 2)
    # match2 = winner(2, 3)
    # match3 = winner(1, 3)

    # A little more general approach
    for p1 in 1:n-1, p2 in p1+1:n
        true ~ Dirac(winner(p1,p2) == p1)
    end

end

num_people = 3
model = tug_of_war(num_people)



num_chns = 4
num_samples = num_people * 10_000 # 10_000

# High rhat
chns = sample(model,  MH(), MCMCThreads(), num_samples, num_chns)

# PG(20) doesn't differentiate the people's pull/strength
# chns = sample(model,  PG(20), MCMCThreads(), num_samples, num_chns)

# chns = sample(model,  HMC(0.1, 5), MCMCThreads(), num_samples, num_chns)

# IS() doesn't differentiate the people's pull/strength
# chns = sample(model,  IS(), MCMCThreads(), num_samples, num_chns)

# High rhat. Alot of rejected proposals
# chns = sample(model,  HMC(0.1, 5), MCMCThreads(), num_samples, num_chns)


# chns = sample(model,  HMCDA(200, 0.65, 0.3), MCMCThreads(), num_samples, num_chns)

# chns = sample(model,  NUTS(1000, 0.65), MCMCThreads(), num_samples, num_chns)

# Too slow
# chns = sample(model,  SMC(), MCMCThreads(), num_samples, num_chns)



# Good: slightly high rhat
# chns = sample(model,  NUTS(1000,0.65), MCMCThreads(), num_samples, num_chns)

display(chns)
display(gelmandiag(chns))
# display(plot(chns))
df = DataFrame(chns)


# Show the latent strength
println("strength (latent):")
for p1 in 1:num_people-1, p2 in p1+1:num_people
    println("strength[$p1] > strength[$p2]: ", mean(df[!,"strength[$p1]"] .>df[!,"strength[$p2]"]))
end

println("pulls (observed):")
for p1 in 1:num_people-1, p2 in p1+1:num_people
    println("pulls[$p1] > pulls[$p2]: ", mean(df[!,"pulls[$p1]"] .>df[!,"pulls[$p2]"]))
end
