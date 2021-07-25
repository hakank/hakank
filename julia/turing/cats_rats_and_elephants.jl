#=
  "Cats and rats and elephants"
  https://www.allendowney.com/blog/2018/12/11/cats-and-rats-and-elephants/
  """
  A few weeks ago I posted 'Lions and Tigers and Bears', 
  [https://www.allendowney.com/blog/2018/12/03/lions-and-tigers-and-bears/]
  which poses a Bayesian problem related to the Dirichlet distribution.  If you have 
  not read it, you might want to start there.

  Now here’s the follow-up question:

    Suppose there are six species that might be in a zoo: lions and tigers and bears, 
    and cats and rats and elephants. Every zoo has a subset of these species, and every 
    subset is equally likely.

    One day we visit a zoo and see 3 lions, 2 tigers, and one bear. Assuming that every 
    animal in the zoo has an equal chance to be seen, what is the probability that the 
    next animal we see is an elephant?
  """

  Note: The approach in this model is the same as in the
  "Lions and Tigers and Bears" problem, just added
    - the three new animals
    - queries which calculates subsets and number of different animals

Distributions of variable animal7
lion       =>    3755  (0.375500)
tiger      =>    3677  (0.367700)
elephant   =>    2205  (0.220500)
bear       =>     323  (0.032300)
cat        =>      21  (0.002100)
rat        =>      19  (0.001900)

Distribution of unique combinations (unsorted)
Distributions of variable (num:0)
[2, 1, 0]	=>	4362 (0.4362)
[2, 0]	=>	3645 (0.3645)
[3, 2, 1, 0]	=>	1540 (0.154)
[1, 2, 0]	=>	324 (0.0324)
[3, 1, 0]	=>	29 (0.0029)
[2, 0, 1]	=>	25 (0.0025)
[1, 0, 2]	=>	18 (0.0018)
[3, 0, 1, 2]	=>	12 (0.0012)
[3, 1, 0, 2]	=>	11 (0.0011)
[4, 0, 2]	=>	5 (0.0005)
[3, 0, 2, 1]	=>	5 (0.0005)
[4, 0, 1]	=>	4 (0.0004)
[1, 0, 3]	=>	3 (0.0003)
[6, 0]	=>	3 (0.0003)
[1, 3, 0, 2]	=>	2 (0.0002)
[4, 1, 0]	=>	2 (0.0002)
[2, 0, 3, 1]	=>	2 (0.0002)
[3, 2, 0, 1]	=>	2 (0.0002)
[1, 3, 0]	=>	2 (0.0002)
[3, 0]	=>	1 (0.0001)
[1]	=>	1 (0.0001)
[2, 0, 1, 3]	=>	1 (0.0001)
[3, 0, 1]	=>	1 (0.0001)

  Cf lions__tiger_and_bears*.jl
     ~/blog/cats_rats_and_elephants.blog 
     ~/webppl/cats_rats_and_elephants.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")


@model function cats_rats_and_elephants()
    #  The animals.
    lion     = 1
    tiger    = 2
    bear     = 3
    cat      = 4
    rat      = 5
    elephant = 6
    
    #  Prior
    #  We draw 6 times with the multinomial distribution.
    #  What is the probability of different combinations of the number of each animal?
    alphas = [1/6, 1/6, 1/6, 1/6, 1/6, 1/6]; #  must sum to 1
    #  alphas = [2/12, 2/12, 2/12, 2/12, 2/12, 2/12];
    
    #  Draw 6 animals
    n = 6
    x ~ Multinomial(n,alphas)

    #  The probabilities to calculate ("aliased" for simplicity)
    probLion      = x[1]
    probTiger     = x[2]
    probBear      = x[3]
    probCat       = x[4]
    probRat       = x[5]
    probElephant  = x[6]
    
    #  Posterior: What is the probability of lion, tiger, and bear given the observations?
    n2 = n+1
    o = tzeros(n2)
    for i in 1:n2
        o[i] ~ Categorical(simplex([probLion,probTiger,probBear,probCat,probRat,probElephant]))
    end

    #  Observations
    true ~ Dirac(o[1] == lion)
    true ~ Dirac(o[2] == lion)
    true ~ Dirac(o[3] == lion)
    true ~ Dirac(o[4] == tiger)
    true ~ Dirac(o[5] == tiger)
    true ~ Dirac(o[6] == bear)

    animal7 ~ Dirac(o[7])
    
    # probbear7     ~ Dirac(o[7] == bear)
    # problion7     ~ Dirac(o[7] == lion)
    # probtiger7    ~ Dirac(o[7] == tiger)
    # probcat7      ~ Dirac(o[7] == cat)
    # probrat7      ~ Dirac(o[7] == rat)
    # probelephant7 ~ Dirac(o[7] == elephant)

    #  Which subset of the animals is the most probable?
    # xunique_ = unique(x)
    # xunique = sort(reshape(xunique_,length(xunique_),1),dims=1)
    xunique = unique(x) # unsorted
    
    #  Number of different animals in the zoo
    numAnimals ~ Dirac(length(xunique))

    return xunique
    
end


model = cats_rats_and_elephants()
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)

#
display(chns)
show_var_dist_pct(chns,:animal7,["lion","tiger","bear","cat","rat","elephant"])

println()
show_var_dist_pct(chns,:probbear7)

# println()
# show_var_dist_pct(chns,:problion7)

# println()
# show_var_dist_pct(chns,:probtiger7)

# println()
# show_var_dist_pct(chns,:probcat7)

# println()
# show_var_dist_pct(chns,:probrat7)

# println()
# show_var_dist_pct(chns,:probelephant7)

println()
show_var_dist_pct(chns,:numAnimals)

println("Distribution of unique combinations (unsorted)")
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)
