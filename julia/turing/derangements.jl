#=

  From http://rosettacode.org/wiki/Permutations/Derangements
  """
  Permutations/Derangements
  A derangement is a permutation of the order of distinct items in which 
  no item appears in its original place.

  For example, the only two derangements of the three items 
    (0, 1, 2) are (1, 2, 0), and (2, 0, 1).
  
  The number of derangements of n distinct items is known as the subfactorial of n, 
  sometimes written as !n. There are various ways to calculate !n.
  ...
  """

  The theoretical value is 1/exp(1): 0.36787944117144233

  n: 2
  Distributions of variable total0 (num:0)
  0.00000 =>    5116  (0.511600)
  1.00000 =>    4884  (0.488400)

  n: 3
  Distributions of variable total0 (num:0)
  0.00000 =>    6821  (0.682100)
  1.00000 =>    3179  (0.317900)

  n: 4
  Distributions of variable total0 (num:0)
  0.00000 =>    6478  (0.647800)
  1.00000 =>    3522  (0.352200)

  n: 5
  Distributions of variable total0 (num:0)
  0.00000 =>    6134  (0.613400)
  1.00000 =>    3866  (0.386600)


  Cf ~/webppl/derangements.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function derangements(n=3)

    p ~ filldist(DiscreteUniform(1,n),n)
    
    #  Ensure unicity.
    for i in 1:n
        for j in i+1:n
            true ~ Dirac(p[i] != p[j])
        end
    end
    
    #  How many are in the i'th position
    total ~ Dirac(sum([i == p[i] for i in 1:n]))
   
    total0 ~ Dirac(total == 0)
    return p
end

function run_derangements(n)
  
    println("n: ", n, " theoretical: ", 1/exp(1))
    model = derangements(n)

    num_chains = 4

    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(10), 10_000)
    # chns = sample(model, IS(), 100_000)

    # display(chns)
    # display(plot(chns))
    
    show_var_dist_pct(chns, :total0)
    # for i in 1:n
    #     show_var_dist_pct(chns, Symbol("p[$i]"))
    # end

    # chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
    # genq = generated_quantities(model, chains_params)
    # show_var_dist_pct(genq)

end

for n in 2:5
    run_derangements(n)
    println()
end

# run_derangements(7)
