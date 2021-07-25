#=
   Football bet Simple.

   Netica model of a football bet.
   From Neapolitan?
   The Netica model use an utility node (u) and a decision node (accept_bet)
   which I try to model here...

   Distributions of variable weather (num:0)
   1.00000 =>   10000  (1.000000)

   Distributions of variable accept_bet (num:0)
   2.00000 =>    5072  (0.507200)
   1.00000 =>    4928  (0.492800)

   Distributions of variable result (num:0)
   1.00000 =>    5924  (0.592400)
   2.00000 =>    4076  (0.407600)

   Distributions of variable u (num:0)
   20.00000 =>    2972  (0.297200)
   40.00000 =>    2952  (0.295200)
   -20.00000 =>    2120  (0.212000)
   -5.00000 =>    1956  (0.195600)

  Distributions of variable u_accept_bet_yes (num:0)
  0.00000 =>    4928  (0.492800)
  40.00000 =>    2952  (0.295200)
  -20.00000 =>    2120  (0.212000)

  [accept_bet,u]
  Distributions of variable (num:0)
  [1, 20]	=>	2972 (0.2972)
  [2, 40]	=>	2952 (0.2952)
  [2, -20]	=>	2120 (0.212)
  [1, -5]	=>	1956 (0.1956)

   Cf ~/blog/football_bet_simple.blog
      ~/webppl/football_bet_simple.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function football_bet_simple()
    wet = 1
    dry = 2

    no = 1
    yes = 2


    melbwins = 1
    melbloses = 2
    
    weather ~ Categorical([0.30,0.70]) # [wet,dry]
    accept_bet ~ Categorical([0.50,0.50]) # [yes,no]
    
    result ~ weather == wet ? Categorical([0.60,0.40]) :  # [melbwins,melbloses]
        Categorical([0.25,0.75])
    
    u ~ Dirac( 
        (result == melbwins  && accept_bet==yes) ?  40 :
        (result == melbwins  && accept_bet==no)  ?  20 : 
        (result == melbloses && accept_bet==yes) ? -20 :
        (result == melbloses && accept_bet==no)  ? -5 : 0;
    )
    
    u_accept_bet_yes ~ Dirac(accept_bet == yes ? u : 0)
    u_accept_bet_no  ~ Dirac(accept_bet == no  ? u : 0)
    
    true ~ Dirac(weather == wet)
    #  true ~ Dirac(result == melbwins)

    return [accept_bet,u]

end

model = football_bet_simple()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :weather)
show_var_dist_pct(chns, :accept_bet)
show_var_dist_pct(chns, :result)
show_var_dist_pct(chns, :u)
show_var_dist_pct(chns, :u_accept_bet_yes)

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
println("[accept_bet,u]")
show_var_dist_pct(genq)




