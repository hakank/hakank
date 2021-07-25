#=
  All different in Turing.jl

  Well, I have to try, don't I. :-)

  With enforce_alldiff true (counting number unique values)

  Distributions of variable num_unique (num:0)
  6.00000 =>   10000  (1.000000)

  With enforce_alldiff false:
  Distributions of variable num_unique (num:0)
  4.00000 =>    5029  (0.502900)
  3.00000 =>    2340  (0.234000)
  5.00000 =>    2295  (0.229500)
  2.00000 =>     197  (0.019700)
  6.00000 =>     138  (0.013800)
  1.00000 =>       1  (0.000100)


  Cf ~/blog/all_different.blog
     ~/webppl/all_different.wppl

=#

using Turing
include("jl_utils.jl")

@model function all_different(n=6,base=10,enforce_alldiff=true)
    # digits = tzeros(n)
    # for i in 1:n
    #     digits[i] ~ DiscreteUniform(0,base-1)
    # end
    # This is much faster: 6.5s vs 33.9s!
    digits ~ filldist(DiscreteUniform(0,base-1), n)
    
    if enforce_alldiff
        for i in 1:n, j in i+1:n
            true ~ Dirac(digits[i] != digits[j])
        end
    end

    # num_duplicates ~ Dirac(sum([
    #                              sum([digits[i] == d for i in 1:n]) > 1
    #                              for d in 0:base-1
    #                             ]))

    # Faster
    num_unique ~ Dirac(length(unique(digits)))
    
    # This helps
    if enforce_alldiff
        # true ~ Dirac(num_duplicates == 0)
        true ~ Dirac(num_unique == n)
    end
    # println("digits: ", digits, " num_unique: ", num_unique)
    
end

n = 6
base = 6
model = all_different(n,base,true)

num_chains = 4
# chains = sample(model, Prior(), 1000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SGLD(), 10_000)

display(chains)
# display(plot(chains))

# show_var_dist_pct(chains, :num_duplicates)
show_var_dist_pct(chains, :num_unique)
