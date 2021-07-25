#=
   Car buyer - Neapolital

   From the Netica model Car_Buyer Neapolital.dne
   """
   This decision network is from Neapolitan90 (p.373), which is a simpler version of the car 
   buyer example of Howard62.  Eliminating the rationale given there of how the numbers are 
   arrived at, we have the following story:

   Joe is going to buy a used car, which could be good with probability 0.8 or a lemon with 
   probability 0.2.  After accounting for repairs, Joe's profit will be $60 if the car is 
   good, and $-100 if it is bad.  Before buying the car he has the option of having one test 
   or two tests done on it.  The first test costs $9, and both together cost $13.  The first 
   test has a 90% chance of returning positive if the car is good, and a 40% chance if it's 
   a lemon.  If the first test returns positive, then the second test has a 88.89% chance of 
   returning positive if the car is good, and a 33.33% chance if it's a lemon.  If the first 
   test returns negative, then the second test has a 100% chance of returning positive if 
   the car is good, and a 44.44% chance if it's a lemon.

   Joe has 2 decisions to make: whether to do the tests, and whether to buy the car.  These 
   are represented by the "Do Tests?" and "Buy It?" decision nodes.  The outcome of the tests 
   are given by nodes "First Test" and "Second Test".  The costs of the tests are represented 
   by utility node U, and the profits after repairs (not including test costs) by utility 
   node V.

   When Joe decides whether to do the tests, he doesn't know the value of any of these 
   variables, so there are no links entering the "Do Tests?" node.  When he decides whether 
   to buy, he will know the outcome of both tests (the outcomes may be "not done"), and so 
   there are links from those two nodes to "Buy It?".  He will also know the value of 
   "Do Tests?" since he has already made that decision, so you could put a link from that 
   node to "Buy It?", but it is not necessary since it is a no-forgetting link and there is 
   already a directed path from "Do Tests?" to "Buy It?".
   """

   Distributions of variable condition
   lemon      =>    5813  (0.581300)
   good       =>    4187  (0.418700)

   Distributions of variable do_tests
   both       =>   10000  (1.000000)

   Distributions of variable first_test
   negative   =>   10000  (1.000000)

   Distributions of variable second_test
   notDone    =>    4187  (0.418700)
   negative   =>    3276  (0.327600)
   positive   =>    2537  (0.253700)

   Distributions of variable buy_it
   dont_buy   =>    5813  (0.581300)
   buy        =>    4187  (0.418700)

   Distributions of variable v (num:0)
   0.00000 =>    5813  (0.581300)
   60.00000 =>    4187  (0.418700)

   Distributions of variable u (num:0)
  -13.00000 =>   10000  (1.000000)

   do_tests, first_test, second_test, buy_it, v, u, v+y:
   Distributions of variable (num:0)
   [3, 3, 1, 1, 60, -13, 47]	=>	4187 (0.4187)
   [3, 3, 3, 2, 0, -13, -13]	=>	3276 (0.3276)
   [3, 3, 2, 2, 0, -13, -13]	=>	2537 (0.2537)


  Cf ~/blog/car_buyer.blog
     ~/webppl/car_buyer.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function car_buyer()
    good = 1
    lemon = 2
    # conditions = [good, lemon]

    none = 1
    first = 2
    both = 3
    # doTests = [none", first", both"]
    
    notDone = 1
    positive = 2
    negative = 3
    # tests = [notDone, positive, negative]

    buy = 1
    dont_buy = 2
    
    condition ~ Categorical(simplex([80,20])) # [good, lemon]
    do_tests ~ Categorical(simplex([28,26.2,22.7333])) # [none,first,both]

    # [notDone,positive,negative]
    first_test ~ 
        (condition==good && do_tests==none)  ? Categorical(simplex([100,0,0])) :
        (condition==good && do_tests==first) ? Categorical(simplex([0,90,10])) :
        (condition==good && do_tests==both)  ? Categorical(simplex([0,90,10])) :
        (condition==lemon&& do_tests==none)  ? Categorical(simplex([100,0,0])) :
        (condition==lemon&& do_tests==first) ? Categorical(simplex([0,40,60])) :
        (condition==lemon&& do_tests==both)  ? Categorical(simplex([0,40,60])) : Dirac(0)
    
    second_test ~ (first_test==notDone && condition==good && do_tests==none)    ? Categorical(simplex([100, 0, 0])) :
        (first_test==notDone && condition==lemon && do_tests==none)   ? Categorical(simplex([100, 0, 0])) :
        
        (first_test==positive && condition==good && do_tests==first)  ? Categorical(simplex([100, 0, 0])) :
        (first_test==positive && condition==good && do_tests==both)   ? Categorical(simplex([0, 88.889, 11.111])) :
        
        (first_test==positive && condition==lemon && do_tests==first) ? Categorical(simplex([100, 0, 0])) :
        (first_test==positive && condition==lemon && do_tests==both)  ? Categorical(simplex([100, 33.3333, 66.6667])) :
        
        (first_test==negative && condition==good && do_tests==first)  ? Categorical(simplex([100, 0, 0])) :
        (first_test==negative && condition==good && do_tests==both)   ? Categorical(simplex([100, 0, 0])) :
        
        (first_test==negative && condition==lemon && do_tests==first) ? Categorical(simplex([100, 0, 0])) :
        (first_test==negative && condition==lemon && do_tests==both)  ? Categorical(simplex([0, 44.4444, 55.5556])) : Dirac(0)
    
    
    buy_it ~ Dirac(
            (do_tests == first && second_test == notDone && first_test == negative)
            ||
            (do_tests == both && second_test == positive && first_test == negative)
            ||
            (do_tests == both && second_test == negative && first_test == positive)
            ||
            (do_tests == both && second_test == negative && first_test == negative) ? dont_buy : buy)
    
    v ~ Dirac(
        (condition==good  && buy_it==buy)      ? 60   :
        (condition==good  && buy_it==dont_buy) ? 0    :
        (condition==lemon && buy_it==buy)      ? -100 :
        (condition==lemon && buy_it==dont_buy) ?  0   : -1
        )
    
    u ~ Dirac(
        do_tests == none  ?  0  :
        do_tests == first ? -9  :
        do_tests == both  ? -13 : -1
    )
    
    
    true ~ Dirac(do_tests == both)
    true ~ Dirac(first_test == negative)
    # true ~ Dirac(second_test == negative)

    # true ~ Dirac(do_tests == first)
    # true ~ Dirac(first_test == negative)
    # true ~ Dirac(buy_it==buy)
    # true ~ Dirac(v>0)
    return [do_tests,first_test,second_test,buy_it,v,u,v+u]
end

model = car_buyer()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,10), 10_000)

display(chns)
# display(plot(chns))

println("\ngood = 1 lemon = 2")
show_var_dist_pct(chns, :condition,["good","lemon"])

println("\nnone = 1 first = 2  both = 3")
show_var_dist_pct(chns, :do_tests,["none","first","both"])

println("\nnotDone = 1 positive = 2 negative = 3")
show_var_dist_pct(chns, :first_test,["notDone","positive","negative"])

show_var_dist_pct(chns, :second_test,["notDone","positive","negative"])

println("\nbuy = 1 dont_buy = 2\n")
show_var_dist_pct(chns, :buy_it,["buy","dont_buy"])

println()
show_var_dist_pct(chns, :v)

println()
show_var_dist_pct(chns, :u)

println()
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
println("do_tests, first_test, second_test, buy_it, v, u, v+y:")
show_var_dist_pct(genq)
