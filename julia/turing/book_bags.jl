#=
   From the Netica model Book Bags.neta
   """
   Book Bags                  Copyright 1998 Norsys Software Corp.

   There are two book bags each containing 10 poker chips.  In one
   bag there are 7 red and 3 blue.  In the other bag there are 3 
   red and 7 blue. Five chips are drawn out of one of the bags and 
   shown to the subject (one at a time then returned to the bag).  
   The subject does not know which bag the chips came from.  
   There is an equal chance that the draws are made from either
   bag. After each draw the subject reports which bag he believes
   the chips are coming from and provides a probability that the 
   chips are being drawn from that bag.
      
   The problem comes from the early "revision of judgment" work 
   that indicated that people were conservative with respect to Bayes.   
   """

   Bag1 (7 blue, 3 red): 1 Bag2 (3 blue, 7 red): 2

   We've seen 1 blue chip(s).
   Distributions of variable bag
   bag1       =>   70262  (0.702620)
   bag2       =>   29738  (0.297380)

   We've seen 2 blue chip(s).
   Distributions of variable bag
   bag1       =>   84680  (0.846800)
   bag2       =>   15320  (0.153200)

   We've seen 3 blue chip(s).
   Distributions of variable bag
   bag1       =>   93152  (0.931520)
   bag2       =>    6848  (0.068480)

   We've seen 4 blue chip(s).
   Distributions of variable bag
   bag1       =>   90562  (0.905620)
   bag2       =>    9438  (0.094380)

   We've seen 5 blue chip(s).
   Distributions of variable bag
   bag1       =>   98388  (0.983880)
   bag2       =>    1612  (0.016120)

   Cf ~/blog/book_bags.blog
      ~/webppl/book_bags.wppl

=#

using Turing
# using Plots, StatsPlots
include("jl_utils.jl")

@model function book_bags(num_blue_chips=2)
    bag1 = 1 # 7 blue 3 red
    bag2 = 2 # 3 blue 7 red

    bag ~ Categorical([0.5,0.5])
    
    blue = 1
    red = 2

    n = 5
    draw = tzeros(num_blue_chips)
    for d in 1:num_blue_chips
        if bag == bag1
            draw[d] ~ Categorical([0.7,0.3])
        else
            draw[d] ~ Categorical([0.3,0.7])
        end
    end

    # We've seen $test number of blue chips
    for d in 1:num_blue_chips
        true ~ Dirac(draw[d] == blue)
    end

end



function run_book_bags(num_blue_chips) 
    println("\nWe've seen $num_blue_chips blue chip(s).")
    model = book_bags(num_blue_chips)
    num_chains = 4
    # chs = sample(model, Prior(), 1000)
    chs = sample(model, MH(), 100_000)
    # chs = sample(model, PG(15), 10_000)
    # chs = sample(model, IS(), 10_000)
    # chs = sample(model, SMC(), 10_000)
    # chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chains)

    # chs = sample(model, SGLD(), 10_000)
    # chs = sample(model,NUTS(), 10_000)
    # chs = sample(model,HMC(0.01,5), 10_000)

    # display(chs)
    # display(plot(chs))

    show_var_dist_pct(chs,:bag,["bag1","bag2"])
    
end

println("Bag1 (7 blue, 3 red): 1 Bag2 (3 blue, 7 red): 2")
for test in 1:5
    run_book_bags(test)
end
