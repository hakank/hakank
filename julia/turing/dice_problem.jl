#=

   Dice problem.

   From Think Bayes, page 21.
   """
   Suppose I have a box of dice that contains a 4-sided die, a 6-sided die, an
   8-sided die, a 12-sided die, and a 20-sided die. If you have ever played
   Dungeons & Dragons, you know what I am talking about.

   Suppose I select a die from the box at random, roll it, and get a 6. What is
   the probability that I rolled each die?

   ...
   
   What if we roll a few more times and get 6, 8, 7, 7, 5, and 4?
   """
  
   As we get more information (i.e. that all values are <= 8) we get more and
   more sure that it's the 8-sided die.

   throws: [6]
   Distributions of variable dice (num:0)
   6.00000 =>    3881  (0.388100)
   8.00000 =>    2965  (0.296500)
   12.00000 =>    1992  (0.199200)
   20.00000 =>    1162  (0.116200)

   throws: [6, 6]
   Distributions of variable dice (num:0)
   6.00000 =>    5217  (0.521700)
   8.00000 =>    3008  (0.300800)
   12.00000 =>    1321  (0.132100)
   20.00000 =>     454  (0.045400)

   throws: [6, 6, 8]
   Distributions of variable dice (num:0)
   8.00000 =>    7438  (0.743800)
   12.00000 =>    2102  (0.210200)
   20.00000 =>     460  (0.046000)

   throws: [6, 6, 8, 7]
   Distributions of variable dice (num:0)
   8.00000 =>    8183  (0.818300)
   12.00000 =>    1615  (0.161500)
   20.00000 =>     202  (0.020200)

   throws: [6, 6, 8, 7, 7]
   Distributions of variable dice (num:0)
   8.00000 =>    8766  (0.876600)
   12.00000 =>    1130  (0.113000)
   20.00000 =>     104  (0.010400)

   throws: [6, 6, 8, 7, 7, 5]
   Distributions of variable dice (num:0)
   8.00000 =>    9163  (0.916300)
   12.00000 =>     792  (0.079200)
   20.00000 =>      45  (0.004500)

   throws: [6, 6, 8, 7, 7, 5, 4]
   Distributions of variable dice (num:0)
   8.00000 =>    9413  (0.941300)
   12.00000 =>     575  (0.057500)
   20.00000 =>      12  (0.001200)


   However, if the next throw is 11 then all previous bets are off:

   throws: [6, 6, 8, 7, 7, 5, 4, 11]
   Distributions of variable dice (num:0)
   12.00000 =>    9851  (0.985100)
   20.00000 =>     149  (0.014900)


   Cf ~/blog/dice_problem.blog
      ~/psi/dice_problem.psi
      ~/webppl/dice_problem.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function dice_problem()
    throws_len = length(throws)
    
    values=[4,6,8,12,20]
    values_len = length(values)

    # Index of the values array
    ix ~ Categorical([1/values_len for _ in 1:values_len])
    # The value of the die
    dice ~ Dirac(values[ix])

    throws ~ filldist(DiscreteUniform(1,dice),throws_len)
    

end

throws=[6,6,8,7,7,5,4,11]

function run_dice_problem(throws)
    println("throws: ", throws)
    model = dice_problem(throws)

    num_chains = 4

    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(10), 10_000)
    # chns = sample(model, IS(), 100_000)

    # display(chns)
    # display(plot(chns))

    show_var_dist_pct(chns,:dice)

end

for i in 1:length(throws)
    run_dice_problem(throws[1:i])
    println()
end
