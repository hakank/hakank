#=
  https://dtai.cs.kuleuven.be/problog/tutorial/basic/06_more_features.html
  """
  Suppose we are packing our bags to go on a trip. We have a set of items, each having a 
  particular weight, and we pack each item with probability inversely proportional to its 
  weight. We want to compute the probability that we will have excess baggage, i.e., 
  that the total weight of our baggage will exceed a given limit. We can model this with 
  the following ProbLog program.
  
  """
  
  Note: This not the same approach as the ProbLog model, it's actually more like MiniZinc than Prolog.


  Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

   selected[1]    0.1588    0.3655     0.0037    0.0052   6215.6490    1.0011     1156.3998
   selected[2]    0.2455    0.4304     0.0043    0.0054   6377.7829    1.0001     1186.5643
   selected[3]    0.3322    0.4710     0.0047    0.0059   6058.6116    1.0000     1127.1836
   selected[4]    0.4870    0.4999     0.0050    0.0060   6763.9998    1.0000     1258.4186
    sumWeights    3.9054    3.3332     0.0333    0.0419   6509.3562    1.0002     1211.0430
  excess_limit    0.1237    0.3293     0.0033    0.0041   6781.6167    1.0001     1261.6961


  Distributions of variable sumWeights (num:0)
  0.00000 =>    2215  (0.221500)
  2.00000 =>    2112  (0.211200)
  3.00000 =>    1072  (0.107200)
  6.00000 =>     992  (0.099200)
  5.00000 =>     965  (0.096500)
  4.00000 =>     708  (0.070800)
  9.00000 =>     623  (0.062300)
  8.00000 =>     373  (0.037300)
  7.00000 =>     326  (0.032600)
  11.00000 =>     207  (0.020700)
  12.00000 =>     146  (0.014600)
  10.00000 =>     132  (0.013200)
  13.00000 =>      76  (0.007600)
  15.00000 =>      53  (0.005300)

  
  Distributions of variable (num:0)
  Any[0.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 0.0], [3.0, 3.0, 0.0], [4.0, 2.0, 0.0]]]	=>	2215 (0.2215)
  Any[2.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 0.0], [3.0, 3.0, 0.0], [4.0, 2.0, 1.0]]]	=>	2112 (0.2112)
  Any[3.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 0.0], [3.0, 3.0, 1.0], [4.0, 2.0, 0.0]]]	=>	1072 (0.1072)
  Any[5.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 0.0], [3.0, 3.0, 1.0], [4.0, 2.0, 1.0]]]	=>	965 (0.0965)
  Any[4.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 1.0], [3.0, 3.0, 0.0], [4.0, 2.0, 0.0]]]	=>	708 (0.0708)
  Any[6.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 1.0], [3.0, 3.0, 0.0], [4.0, 2.0, 1.0]]]	=>	606 (0.0606)
  Any[9.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 1.0], [3.0, 3.0, 1.0], [4.0, 2.0, 1.0]]]	=>	408 (0.0408)
  Any[6.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 0.0], [3.0, 3.0, 0.0], [4.0, 2.0, 0.0]]]	=>	386 (0.0386)
  Any[8.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 0.0], [3.0, 3.0, 0.0], [4.0, 2.0, 1.0]]]	=>	373 (0.0373)
  Any[7.0, [[1.0, 6.0, 0.0], [2.0, 4.0, 1.0], [3.0, 3.0, 1.0], [4.0, 2.0, 0.0]]]	=>	326 (0.0326)
  Any[9.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 0.0], [3.0, 3.0, 1.0], [4.0, 2.0, 0.0]]]	=>	215 (0.0215)
  Any[11.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 0.0], [3.0, 3.0, 1.0], [4.0, 2.0, 1.0]]]	=>	207 (0.0207)
  Any[12.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 1.0], [3.0, 3.0, 0.0], [4.0, 2.0, 1.0]]]	=>	146 (0.0146)
  Any[10.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 1.0], [3.0, 3.0, 0.0], [4.0, 2.0, 0.0]]]	=>	132 (0.0132)
  Any[13.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 1.0], [3.0, 3.0, 1.0], [4.0, 2.0, 0.0]]]	=>	76 (0.0076)
  Any[15.0, [[1.0, 6.0, 1.0], [2.0, 4.0, 1.0], [3.0, 3.0, 1.0], [4.0, 2.0, 1.0]]]	=>	53 (0.0053)


  Cf ~/blog/trip.blog
     ~/webppl/trip.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function trip()
    limit = 8
    # limit = 4.0
    # limit = 10.0
    # limit = 18.0

    skis = 1
    boots = 2
    helmet = 3
    gloves = 4
    items = [skis,boots,helmet,gloves]

    weights = Dict(skis=> 6,
                   boots=> 4,
                   helmet=> 3,
                   gloves=> 2
                  )
    n = 4
    selected = tzeros(n)
    for i in 1:n
        selected[i] ~ flip(1/weights[i])
    end
    
    sumWeights ~ Dirac(sum([weights[i]*selected[i] for i in 1:n]))

    # The probability the we excess the weight.
    function excess(v) 
        return sumWeights > v
    end
    
    # Here we can enforce that we don't excess the weights
    # true ~ Dirac(sumWeights > 0 && excess(limit) == false)

    # [name,weight,selected]
    items_with_weights = [sumWeights, [[i,weights[i],selected[i]] for i in 1:n]]

    excess_limit ~ Dirac(excess(limit))
    
    return items_with_weights
end

model = trip()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:sumWeights)


chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)
