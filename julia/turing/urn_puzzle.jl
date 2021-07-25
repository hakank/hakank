#=
   Urn puzzle in Turing.jl
    
   From Berlin Bayesians
   https://app.slack.com/client/TFPMSKW3F/CFQHMRD6K/thread/CFQHMRD6K-1623812230.000500
   """
   Three urns, first blindly take one from the first and put in the second, 
   then blindly take one from the second and put in the third and then blindly pick 
   one from the third. How likely is it to pick a black one in the last step?

   [
     Urns

     1: white black black
     2: white white black
     3: white black

   ]
   """

   Probability of drawing a black in the last step:
   Distributions of variable (num:0)
   0.00000 =>    5273  (0.527300)
   1.00000 =>    4727  (0.472700)

   (Cf the WebPPL model's exact solution: 0.47222222222222227)
   
   Cf ~/webppl/urn_puzzle
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model urn_puzzle() = begin
    white = 1
    black = 2

    urn1 = [white,black,black]
    urn2 = [white,white,black]
    urn3 = [white,black]

    # From urn 1 to urn 2
    # Note: We have to do this in two steps, i.e.
    # first pick some integer and then fetch that p1'th ball
    # from the urn
    p1 ~ DiscreteUniform(1,length(urn1))
    urn2b = push!(urn2,urn1[p1])

    # From urn 2 to urn 3
    p2 ~ DiscreteUniform(1,length(urn2b))
    urn3b = push!(urn3,urn2b[p2])

    # Pick a ball from urn 3
    p3 ~ DiscreteUniform(1,length(urn3b))
    ball = urn3b[p3]

    # Is it black?
    pblack ~ Dirac(ball==black)

end

model = urn_puzzle()

num_chains = 4

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(15), 10_000)
# chains = sample(model, SMC(1000), 10_000)
# chains = sample(model, IS(), 10_000)
#

display(chains)
# display(plot(chains))

show_var_dist_pct(chains,:pblack)
show_var_dist_pct(chains,:ball)
