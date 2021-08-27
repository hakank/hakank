#=
    Generating Hypergeometric distribution

    https://en.wikipedia.org/wiki/Hypergeometric_distribution
    """
    [T]he probability of k successes (random draws for which the object 
    drawn has a specified feature) in n draws, without replacement, from 
    a finite population of size N that contains exactly K objects with 
    that feature, wherein each draw is either a success or a failure. 
    In contrast, the binomial distribution describes the probability of 
    k successes in n draws with replacement. 
    """


    The example from
    https://en.wikipedia.org/wiki/Hypergeometric_distribution#Working_example
    """
    Now, assume (for example) that there are 5 green and 45 red marbles in 
    the urn. Standing next to the urn, you close your eyes and draw 10 marbles 
    without replacement. What is the probability that exactly 4 of the 10 
    are green? 
    
    The probability of drawing exactly 4 marbels is: ~0.00396458305801506542
    The probability of drawing exactly 5 marbels is: ~0.00011893749174045196
    """

    Since the probabilities are so small, we use big() in the calculations.
    Note that gpmf and gmean are the exact values from the formulas,
    while g and gCount are simulated from the function hypergeometric(.).
    
    * k = 4
        Summary Statistics
        parameters       mean        std   naive_se       mcse          ess       rhat   ess_per_sec 
            Symbol   BigFloat   BigFloat   BigFloat   BigFloat     BigFloat   BigFloat      BigFloat 

                g     0.0036     0.0601     0.0003     0.0003   49813.4280     1.0000    97291.8515
             gpmf     0.0040     0.0000     0.0000     0.0000          NaN        NaN           NaN
           gCount     0.9935     0.8567     0.0038     0.0038   50059.3725     1.0000    97772.2120
            gmean     1.0000     0.0000     0.0000     0.0000     100.7029     1.0000      196.6854
             gvar     0.0000     0.0000     0.0000     0.0000          NaN        NaN           NaN


        Distributions of variable gCount (num:0)
        1.00000 =>   21503  (0.430060)
        0.00000 =>   15703  (0.314060)
        2.00000 =>   10430  (0.208600)
        3.00000 =>    2155  (0.043100)
        4.00000 =>     200  (0.004000)
        5.00000 =>       9  (0.000180)

        mean(g): BigFloat[0.003619999999999999999999999999999999999999999999999999999999999999999999999999983]
        mean(gpmf): BigFloat[0.003964583058015065664125842204157379455864429473876953125]
        mean(gmean): BigFloat[1.0]
        mean(gvar): BigFloat[0.0]


    * k = 5

        parameters       mean        std   naive_se       mcse          ess       rhat   ess_per_sec 
            Symbol   BigFloat   BigFloat   BigFloat   BigFloat     BigFloat   BigFloat      BigFloat 

                g     0.0001     0.0110     0.0000     0.0000   50014.6732     1.0000   106414.1984
             gpmf     0.0001     0.0000     0.0000     0.0000     100.7029     1.0000      214.2615
           gCount     0.9991     0.8551     0.0038     0.0037   50011.1699     1.0000   106406.7445
            gmean     1.0000     0.0000     0.0000     0.0000     100.7029     1.0000      214.2615
             gvar     0.0000     0.0000     0.0000     0.0000          NaN        NaN           NaN


        Distributions of variable gCount (num:0)
        1.00000 =>   21567  (0.431340)
        0.00000 =>   15521  (0.310420)
        2.00000 =>   10566  (0.211320)
        3.00000 =>    2134  (0.042680)
        4.00000 =>     208  (0.004160)
        5.00000 =>       4  (0.000080)

        mean(g): BigFloat[0.0001200000000000000000000000000000000000000000000000000000000000000000000000000003]
        mean(gpmf): BigFloat[0.0001189374917404519634185622312116947796312160789966583251953125]
        mean(gmean): BigFloat[1.0]
        mean(gvar): BigFloat[0.0]

   Cf https://github.com/distributions-io/hypergeometric-random/blob/master/lib/number.js
   
    

   See also hypergeometric_dist.wppl
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

#=
   Hypergeometric:
   What is the probability that we draw exactly k "success" objects
   of the n drawn objects of total N objects where there are in total
   K "success" objects
  
   k: number of successes we want to check
   N: total number of objects
   K: total number of success objects
   n: number of draws
  
=#
function hypergeometric1(k,N,K,n,count)
    if n == 0 || K <= 0
        return count
    else 
        #   we have K successes left and N objects left
        p = K/N #  probability of drawing a success object
        ff = rand(Bernoulli(p))
        if ff == 1.0
            #  We drew a success:
            #  - decrement the total objects (N)
            #  - decrement the number of "success" objects (K)
            #  - decrement the number of drawn objects (n)
            #  - increment the number of successful draws (count)
            return hypergeometric1(k,N-1,K-1,n-1,count+1)
        else
            #  We drew a failure:
            #  - decrement the total objects (N)
            #  - decrement the number of drawn objects (n)
            return hypergeometric1(k,N-1,K,n-1,count)
        end
    end
    
end

function hypergeometric(k,N,K,n)
    res = hypergeometric1(k,N,K,n,0)
    return res == k
end

#
# Return the number of found successes.
#
function hypergeometricCount(k,N,K,n)
    res = hypergeometric1(k,N,K,n,0)
    return res
end

# PDF, From https://en.wikipedia.org/wiki/Hypergeometric_distribution
# Gives the exact probability mass function 
function hypergeometric_pmf(N,K,n,k)
    return  binomial(K,k)*binomial(N-K,n-k) / binomial(N,n)
end

# Exact probability 
# From https://en.wikipedia.org/wiki/Hypergeometric_distribution
function hypergeometric_mean(N,K,n)
    n*K/N
end

# From https://en.wikipedia.org/wiki/Hypergeometric_distribution
function hypergeometric_variance(N,K,n)
    # n{K \over N}{(N-K) \over N}{N-n \over N-1}
    n*binomial(K,N)*binomial(N-K,N)*binomial(N-n,N-1)
end

@model function hypergeometric_dist_test()
    #  total: 5 green and 45 red marbles
    #  drawn: 4 green marbles, 6 red marbles
    K = 5 #  total green marbles: 4 drawn + 1 not drawn
    N = 50 #  total marbles: 5 green + 45 red marbles
    
    k = 4 #  drawn green_marbles
    # k = 5 #  drawn green_marbles    
    n = 10 #  total drawn green + red marbles

    # Note: we use big float since the probabilities 
    # are so small.
    g ~ Dirac(big(hypergeometric(k,N,K,n)))
    gpmf ~ Dirac(big(hypergeometric_pmf(N,K,n,k))) # Exact 
    gCount ~ Dirac(hypergeometricCount(k,N,K,n)) #  Count version
    gmean ~ Dirac(big(hypergeometric_mean(N,K,n))) 
    gvar  ~ Dirac(big(hypergeometric_variance(N,K,n)))
end


model = hypergeometric_dist_test()

# We can use Prior since there's not any observations.
chns = sample(model, Prior(), 50_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5),  10_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)

show_var_dist_pct(chns,:gCount)
println()
println("mean(g): $(mean(chns[[:g]].value.data,dims=1))")
println("mean(gpmf): $(mean(chns[[:gpmf]].value.data,dims=1))")
println("mean(gmean): $(mean(chns[[:gmean]].value.data,dims=1))")
println("mean(gvar): $(mean(chns[[:gvar]].value.data,dims=1))")
