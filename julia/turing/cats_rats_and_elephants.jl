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


Animals lion (1) tiger (2) bear (3) cat (4) rat (5) elephant (6)
Distributions of variable animal7 (num:0)
1.00000 =>    8155  (0.815500)
3.00000 =>     567  (0.056700)
2.00000 =>     357  (0.035700)
5.00000 =>     331  (0.033100)
6.00000 =>     320  (0.032000)
4.00000 =>     270  (0.027000)

Distributions of variable probbear7 (num:0)
0.00000 =>    9433  (0.943300)
1.00000 =>     567  (0.056700)

Distributions of variable problion7 (num:0)
1.00000 =>    8155  (0.815500)
0.00000 =>    1845  (0.184500)

Distributions of variable probtiger7 (num:0)
0.00000 =>    9643  (0.964300)
1.00000 =>     357  (0.035700)

Distributions of variable probcat7 (num:0)
0.00000 =>    9730  (0.973000)
1.00000 =>     270  (0.027000)

Distributions of variable probrat7 (num:0)
0.00000 =>    9669  (0.966900)
1.00000 =>     331  (0.033100)

Distributions of variable probelephant7 (num:0)
0.00000 =>    9680  (0.968000)
1.00000 =>     320  (0.032000)

Distributions of variable numAnimals (num:0)
3.00000 =>    9374  (0.937400)
4.00000 =>     461  (0.046100)
2.00000 =>     138  (0.013800)
1.00000 =>      27  (0.002700)

Distributions of variable (num:0)
[2, 1, 0]	=>	5628 (0.5628)
[3, 1, 0]	=>	2495 (0.2495)
[2, 0, 1]	=>	313 (0.0313)
[1, 2, 0]	=>	257 (0.0257)
[1, 0, 2]	=>	245 (0.0245)
[3, 0, 1, 2]	=>	113 (0.0113)
[2, 0]	=>	106 (0.0106)
[4, 0, 1]	=>	98 (0.0098)
[3, 0, 1]	=>	93 (0.0093)
[1, 0, 3]	=>	91 (0.0091)
[3, 0, 2, 1]	=>	90 (0.009)
[1, 3, 0]	=>	43 (0.0043)
[2, 0, 3, 1]	=>	40 (0.004)
[2, 0, 1, 3]	=>	39 (0.0039)
[4, 1, 0]	=>	38 (0.0038)
[3, 1, 0, 2]	=>	36 (0.0036)
[3, 2, 0, 1]	=>	36 (0.0036)
[3, 0]	=>	27 (0.0027)
[4, 0, 2]	=>	27 (0.0027)
[1]	=>	27 (0.0027)
[2, 1, 0, 3]	=>	22 (0.0022)
[2, 3, 1, 0]	=>	19 (0.0019)
[1, 0, 4]	=>	17 (0.0017)
[3, 2, 1, 0]	=>	13 (0.0013)
[4, 2, 0]	=>	12 (0.0012)
[1, 0, 3, 2]	=>	11 (0.0011)
[1, 3, 0, 2]	=>	8 (0.0008)
[2, 1, 3, 0]	=>	7 (0.0007)
[1, 0, 2, 3]	=>	7 (0.0007)
[2, 3, 0, 1]	=>	7 (0.0007)
[5, 0, 1]	=>	6 (0.0006)
[2, 0, 4]	=>	6 (0.0006)
[1, 2, 0, 3]	=>	6 (0.0006)
[3, 1, 2, 0]	=>	6 (0.0006)
[6, 0]	=>	5 (0.0005)
[1, 4, 0]	=>	4 (0.0004)
[5, 1, 0]	=>	1 (0.0001)
[1, 2, 3, 0]	=>	1 (0.0001)


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
    
    probbear7     ~ Dirac(o[7] == bear)
    problion7     ~ Dirac(o[7] == lion)
    probtiger7    ~ Dirac(o[7] == tiger)
    probcat7      ~ Dirac(o[7] == cat)
    probrat7      ~ Dirac(o[7] == rat)
    probelephant7 ~ Dirac(o[7] == elephant)

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
println("Animals lion (1) tiger (2) bear (3) cat (4) rat (5) elephant (6)")
show_var_dist_pct(chns,:animal7)

println()
show_var_dist_pct(chns,:probbear7)

println()
show_var_dist_pct(chns,:problion7)

println()
show_var_dist_pct(chns,:probtiger7)

println()
show_var_dist_pct(chns,:probcat7)

println()
show_var_dist_pct(chns,:probrat7)

println()
show_var_dist_pct(chns,:probelephant7)

println()
show_var_dist_pct(chns,:numAnimals)

println("Distribution of unique combinations (unsorted)")
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)
