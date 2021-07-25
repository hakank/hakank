#=

   https://reference.wolfram.com/language/ref/BinomialDistribution.html
   """
   Two players roll dice. If the total of both numbers is less than 10, the second player 
   is paid 4 cents; otherwise the first player is paid 9 cents. Is the game fair?:

   ...

   [The game is not fair: mean scores are 1.5 and 3.33333.]

   """

   Model 1: Is the game fair?
   mean L: 10.0
   mean prob1: 0.16558055555555556
   mean prob2: 0.8344194444444445
   mean prob1Times9: 1.490225
   mean prob2Times4: 3.337677777777778
   mean total: -17.7128

   Model 2: What L would be fair?
   mean L: 7.0457
   mean prob1: 0.5409611111111111
   mean prob2: 0.4590388888888889
   mean prob1Times9: 4.86865
   mean prob2Times4: 1.8361555555555555
   mean total: 6.9705

   Note: Model 2 (checkFair==true) is NOT correct.
   In this Turing model, prob1Times9 and prob2Time2 are too different 
   (4.8 vs 1.8) to be considered the same! I'm not sure why the constraint
        true ~ Dirac(prob1Times9 == prob2Times4)
   don't work as expected....

   According to the WebPPL model, the optimal L is 9 (not 7)
   and prob1Times9 ~ prob2Times4.
   Here are the expected values from the WebPPL model:
    [ 'total', -5.998999999999998 ],
    [ 'prob1', 0.3333333333333333 ],
    [ 'prob2', 0.75 ],
    [ 'prob1Times9', 3 ],
    [ 'prob2Times4', 3 ],
    [ 'L', 8.9286 ] ]


   Cf ~/webppl/binomial_dice2.wppl

=#

using Turing
include("jl_utils.jl")

@model function binomial_coin(checkFair=false)
    n = 36
    
    # Limit (optimal if checkFair==true)
    L ~ DiscreteUniform(2,12)
    
    player1 ~ filldist(DiscreteUniform(1,6), n)
    player2 ~ filldist(DiscreteUniform(1,6), n)    
       
    #  Outcome (seen as player1)
    outcome = tzeros(n)
    for i in 1:n
        if player1[i] + player2[i] < L
            outcome[i] ~ Dirac(-4)
        else
            outcome[i] ~ Dirac(9)
        end
    end

    # Probabilities given the limit L
    prob1 ~ Dirac(sum([player1[i] + player2[i] >= L for i in 1:n]) / n)
    prob2 ~ Dirac(sum([player1[i] + player2[i] < L for i in 1:n]) / n)    

    # The total for L games
    total ~ Dirac(sum(outcome[i] for i in 1:L))
    
    prob1Times9 ~ Dirac(prob1*9)
    prob2Times4 ~ Dirac(prob2*4)

    if checkFair
        # What is L for a fair game?
        # This don't give the expected answer
        # true ~ Dirac(prob1Times9 == prob2Times4)
        true ~ Dirac(L == 8.9)
    else
        # The given
        true ~ Dirac(L == 10)
    end
    
end

println("Model 1: Is the game fair?")
model = binomial_coin(false)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(5), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

# display(chs)
# display(plot(chs))

# probs = [:L, :prob1,:prob2,:prob1B,:prob2B,:prob1Times9,:prob2Times4,:total]
probs = [:L, :prob1,:prob2,:prob1Times9,:prob2Times4,:total]
for p in probs
    chsp = chs[p]
    println("mean ", p,": ", mean(chsp))
end


println("\nModel 2: What L would be fair?")
model = binomial_coin(true)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

# display(chs)
# display(plot(chs))


for p in probs
    chsp = chs[p]
    println("mean ", p,": ", mean(chsp))
end
