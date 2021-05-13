#=
  Bertrand's paradox

  From https://towardsdatascience.com/five-paradoxes-with-probabilities-that-will-puzzle-you-2f71201d6ee8
  """
  3. Bertrand's Box Paradox

  If you are familiar with the Montey Hall problem, this paradox is quite 
  similar. In front of us there are three boxes:

    [image of three boxes]

  One box contains two silver coins, one box contains two gold coins and 
  one box contains a gold- and a silver coin. We do not know which coins 
  are in which box. Now, we pick a random box and blindly draw a coin from 
  our box. It’s a gold coin!

  Now, the question is:

    What’s the probability that the second coin in our box is also a gold coin?

  My naive (and wrong) answer when encountering the problem for the first 
  time was ½. I thought that because we drew a gold coin, our box is either 
  the one with the two gold coins or the one with the mixed coins. In the 
  first case, we would draw another gold coin and in the second case, we 
  wouldn’t. Therefore, I presumed the probability should be ½.

    The real probability is ⅔.

  The reason for that is that the first gold coin we drew could either be 
  the only gold coin in the mixed box, the first gold coin in the solely 
  golden box, or the second gold coin in the solely golden box. And in 
  two of these three possibilities, we will draw another gold coin.

  """

  Here we define the boxes as:
  - box1: 2 gold coins
  - box2: 1 gold and 1 silver coin
  - box3: 2 silver coins

  Note: MH() has problem with this. Use SMC() instead.

  julia> @time include("bertrands_paradox.jl")
"""
Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

         box    1.3327    0.4712     0.0024    0.0033   19928.8307    0.9999     9802.6713
       coin1    1.0000    0.0000     0.0000    0.0000          NaN       NaN           NaN
       coin2    1.3327    0.4712     0.0024    0.0033   19928.8307    0.9999     9802.6713

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
      Symbol   Float64   Float64   Float64   Float64   Float64 

         box    1.0000    1.0000    1.0000    2.0000    2.0000
       coin1    1.0000    1.0000    1.0000    1.0000    1.0000
       coin2    1.0000    1.0000    1.0000    2.0000    2.0000

Distributions of variable coin1 (num:0)
1.00000 =>   40000  (1.000000)
Distributions of variable coin2 (num:0)
1.00000 =>   26693  (0.667325)
2.00000 =>   13307  (0.332675)
Distributions of variable box (num:0)
1.00000 =>   26693  (0.667325)
2.00000 =>   13307  (0.332675)
"""
  
  I.e. probability that the second coin is also gold is 0.667325 (~ 2/3).


  Cf ~/blog/bertrands_paradox.blog
      ~/psi/bertrands_paradox.psi

=#

# using Memoization
using Turing # , StatsPlots, DataFrames
# using ReverseDiff, Zygote, Tracker
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)
include("jl_utils.jl")


@model function bertrands_paradox()
    # There are 3 boxes.
    b1 = 1
    b2 = 2
    b3 = 3

    # We select one of the boxes
    box ~ DiscreteUniform(1,3)

    # the coins
    gold = 1
    silver = 2

    # We draw one coin: 
    coin1 ~ (box == b1) ? Categorical(simplex([2.0, 0.0])) :
            (box == b2) ? Categorical(simplex([1, 1])) :
            Categorical(simplex([0.0, 2.0]))

    # It's a gold coin.
    true ~ Dirac(coin1 == gold)

    # What is the probability that the second coin
    # in the selected box is also a gold coin?
    coin2 ~ (box == b1) ? Categorical(simplex([1.0, 0.0])) :
            (box == b2) ? Categorical(simplex([0.0, 1.0])) :
            Categorical(simplex([0.0, 1.0]))

end

model = bertrands_paradox()
num_chains = 4


# chains = sample(model, Prior(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, MH(), 10_000)
# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, PG(20), 1_000)

# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, IS(), 10_000)

# chains = sample(model, SMC(), 10_000)
chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)

# chains = sample(model, NUTS(1000,0.65), MCMCThreads(), 40_000, num_chains)
# chains = sample(model, Gibbs(MH(:zlabels),NUTS(1000,0.65,:m,:b,:sigma)), MCMCThreads(), 40_000, num_chains)

display(chains)

show_var_dist_pct(chains, :coin1)
show_var_dist_pct(chains, :coin2)
show_var_dist_pct(chains, :box)
