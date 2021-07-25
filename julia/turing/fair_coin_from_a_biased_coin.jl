#=
   Fair coin from a biased coin.

   http://cplint.eu/e/von_neumann_trick.swinb
   """
   If you have a biased coin, how to obtain a fair coin, i.e., from a coin that lands heads with 
   probability p with p≠0.5, how to generate samples from {heads,tails} with P(heads)=P(tails)=0.5?

   John von Neuamnn gave the following solution

    1) Toss the coin twice.
    2) If you get HT return H, if you get TH return T
    3) Otherwise, return to 1 and start over

   In fact, if p is the probability of the biased coin landing heads, then the outcomes HT and TH are 
   equiprobable with probability p(1−p). However, with probability pp+(1−p)(1−p)
   these results are not obtained. In this case, we simply repeat the process. The probability that we 
   never get HT or TH is 0 so this procedure ends with certainty.

   See   
   - https://en.wikipedia.org/wiki/Fair_coin#Fair_results_from_a_biased_coin
   - von Neumann, John (1951). "Various techniques used in connection with random digits". 
     National Bureau of Standards Applied Math Series. 12: 36.
   """

   Distributions of variable biased_coin1 (num:0)
   1.00000 =>    7992  (0.799200)
   2.00000 =>    2008  (0.200800)

   Distributions of variable coin1 (num:0)
   1.00000 =>    5004  (0.500400)
   2.00000 =>    4996  (0.499600)

   Cf ~/blog/fair_coin_from_a_biased_coin.blog
      ~/webppl/fair_coin_from_a_biased_coin.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function fair_coin_from_a_biased_coin()
    head = 1
    tail = 2

    n = 3
    function biased_coin(i)
        Categorical([0.8,0.2]) # [head,tail]
    end

    function coin(i)
        # StackOverflowError unless we limit the recursion.
        # But that don't help since we got a lot of 0's as the result....
        # This is fixed below...
        if i > 100
           return Dirac(0)
        end
        bi ~ biased_coin(i)
        bi1 ~ biased_coin(i+1)
        if bi != bi1
            Dirac(bi)
        else
            ci2 ~ coin(i+2)
            Dirac(ci2)
        end
    end

   
    biased_coin1 ~ biased_coin(1)
    coin1 ~ coin(1)
    # This is the fix. Not pretty though!
    true ~ Dirac(coin1 != 0)

  
end

model = fair_coin_from_a_biased_coin()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :biased_coin1)
show_var_dist_pct(chns, :coin1)
    
