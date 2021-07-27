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

  Distributions of variable coin1
  gold       =>    1000  (1.000000)
  Distributions of variable coin2
  gold       =>     668  (0.668000)
  silver     =>     332  (0.332000)
  Distributions of variable box
  b1         =>     668  (0.668000)
  b2         =>     332  (0.332000)

  I.e. probability that the second coin is also gold is 0.668000 (~ 2/3).


  Cf ~/blog/bertrands_paradox.blog
      ~/psi/bertrands_paradox.psi

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


# We observe that the first coin is a gold coin 
@model function bertrands_paradox(coin1=1)
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
    # true ~ Dirac(coin1 == gold)

    # What is the probability that the second coin
    # in the selected box is also a gold coin?
    coin2 ~ (box == b1) ? Categorical(simplex([1.0, 0.0])) :
            (box == b2) ? Categorical(simplex([0.0, 1.0])) :
            Categorical(simplex([0.0, 1.0]))

end

model = bertrands_paradox()
num_chns = 4


# chns = sample(model, Prior(), 10_000, num_chns)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)



display(chns)

show_var_dist_pct(chns, :coin1,["gold","silver"])
show_var_dist_pct(chns, :coin2,["gold","silver"])
show_var_dist_pct(chns, :box,["b1","b2","b3"])
