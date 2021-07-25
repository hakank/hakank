#=
   Dice puzzle in Turing.jl
    
   From Berlin Bayesians:
   (https://app.slack.com/client/TFPMSKW3F/CFQHMRD6K)
   """
   What's likelier: Rolling at least one six in four throws of a single die,
   or rolling at least one double 6 in 24 throws of a pair of dice?
   """
    
   This is de Méré's classical dice puzzle (which induced the study of
   probability theory). See
   - https://mathworld.wolfram.com/deMeresProblem.html

   * 4 throws getting at least one 6_
     1-(5/6)**4  ~ 0.51774691358024691358

   * 24 throws getting at least one double 6
     1-(35/36)**24 ~ 0.49140387613090325958

   Puzzle 1:
   Distributions of variable p (num:0)
   1.00000 =>    5162  (0.516200)
   0.00000 =>    4838  (0.483800)

   Distributions of variable s (num:0)
   0.00000 =>    4838  (0.483800)
   1.00000 =>    3803  (0.380300)
   2.00000 =>    1183  (0.118300)
   3.00000 =>     173  (0.017300)
   4.00000 =>       3  (0.000300)

   Puzzle 2:
   Distributions of variable p (num:0)
   0.00000 =>    5014  (0.501400)
   1.00000 =>    4986  (0.498600)

   Distributions of variable s (num:0)
   0.00000 =>    5014  (0.501400)
   1.00000 =>    3571  (0.357100)
   2.00000 =>    1129  (0.112900)
   3.00000 =>     248  (0.024800)
   4.00000 =>      37  (0.003700)
   5.00000 =>       1  (0.000100)

   Cf ~/webppl/dice_puzzle.jl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model dice_puzzle1() = begin
    n = 4
    throws = tzeros(n)
    for t in 1:n
        throws[t] ~ DiscreteUniform(1,6)
    end
    s ~ Dirac(sum(throws[t] == 6 for t in 1:n))
    p ~ Dirac(s >= 1)
end

@model dice_puzzle2() = begin
    n = 24
    throws = tzeros(n,2)
    for t in 1:n
        throws[t,1] ~ DiscreteUniform(1,6)
        throws[t,2] ~ DiscreteUniform(1,6)        
    end
    s ~ Dirac(sum(throws[t,1] == 6 && throws[t,2] == 6 for t in 1:n))
    p ~ Dirac(s >= 1)
end

function run_model(model) 

    num_chains = 4

    # chains = sample(model(), Prior(), 10_000)
    # chains = sample(model(), MH(), 10_000)
    # chains = sample(model(), PG(15), 10_000)
    # chains = sample(model(), SMC(), 10_000)
    chains = sample(model(), IS(), 10_000)
    #

    # display(chains)
    # display(plot(chains))
    show_var_dist_pct(chains,:p)
    show_var_dist_pct(chains,:s)    
    
end

println("Puzzle 1:")
@time run_model(dice_puzzle1)

println("\n\nPuzzle 2:")
@time run_model(dice_puzzle2)
