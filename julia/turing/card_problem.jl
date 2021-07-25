#=

   From https://math.stackexchange.com/questions/513250/conditional-probability-question-with-cards-where-the-other-side-color-is-to-be
   """
   A box contains three cards. One card is red on both sides, one card is green on both sides, 
   and one card is red on one side and green on the other. One card is selected from the 
   box at random, and the color on one side is observed. If this side is green, what is 
   the probability that the other side of the card is also green?

   ... the answer to this question is 2/3.
   """

   Give that the color of card is green, then the probability 
   of the different cards are:

    all_red: 1 all_green: 2 red_green: 3
    Distributions of variable selected_card (num:0)
    2.00000 =>    6672  (0.667200)
    3.00000 =>    3328  (0.332800)

   I.e. the probability that it's the all green card is 2/3.

   Cf ~/webppl/card_problem.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function card_problem()
    red = 1
    green = 2
    
    all_red = 1
    all_green = 2
    red_green = 3
    
    #  What card did we select?
    selected_card ~ Categorical(simplex([1/3,1/3,1/3])) # [all_red,all_green,red_green]
    
    #  What is the color of the card (one side) that we see?
    card ~ 
        selected_card == all_red   ?  Dirac(red) :
        selected_card == all_green ?  Dirac(green) :
        # [red,green]
        selected_card == red_green ?  Categorical([1/2,1/2]) : Dirac(0)

    # Alternative:
    # if selected_card == all_red
    #     card ~  Dirac(red)
    # elseif selected_card == all_green
    #     card ~ Dirac(green)
    # else
    #     card ~ Categorical([1/2,1/2])
    # end
    
    
    #  The color of the card we see is green
    true ~ Dirac(card == green)

end

model = card_problem()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(10_000), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, HMC(0.1,10), 10_000)

display(chns)
# display(plot(chns))

println("all_red: 1 all_green: 2 red_green: 3")
show_var_dist_pct(chns, :selected_card)
