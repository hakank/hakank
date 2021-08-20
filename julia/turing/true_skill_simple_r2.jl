#=

    This is a port of the R2 model TrueSkillSimple.cs

    With the datafiles
    - true_skill_simple_r2_players.csv
    - true_skill_simple_r2_games.csv

    Output from the R2 model (skills):
    ```
    [0] Mean: 107.265          skills[0]
    [0] Variance: 72.9736

    [1] Mean: 100.541          skills[1]
    [1] Variance: 86.9287

    [2] Mean: 95.7907          skills[2]
    [2] Variance: 84.9168
    ```

    Output from this Turing.jl model:

        parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
            Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

         skills[1]   105.8074    8.9089     0.0891    0.2314   1273.9109    1.0009      155.4119
         skills[2]    99.9198    8.8072     0.0881    0.2444   1261.0051    0.9999      153.8374
         skills[3]    94.0010    9.2404     0.0924    0.2506   1305.8709    0.9999      159.3108
  performance[1,1]   112.9550   14.6471     0.1465    0.4051   1246.4095    0.9999      152.0568
  performance[1,2]    92.8609   14.5794     0.1458    0.4111   1205.0434    1.0000      147.0103
  performance[2,1]   107.0177   14.7617     0.1476    0.3511   1777.9260    1.0001      216.8996
  performance[2,2]    86.8285   14.6496     0.1465    0.3437   1802.4896    1.0000      219.8963
  performance[3,1]   111.3551   15.1643     0.1516    0.2474   2981.5172    1.0002      363.7327
  performance[3,2]    88.0326   15.0679     0.1507    0.3018   2847.2484    1.0005      347.3525


    Cf true_skill.jl

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV, DataFrames
include("jl_utils.jl")

@model function true_skill_simple_r2(players,games) 
    num_players = length(players)
    num_games, m = size(games)

    skills ~ filldist(Normal(100,10),num_players)

    performance = Matrix{Real}(undef, num_players,2) 
    for i in 1:num_games 
        performance[i,1] ~ Normal(skills[games[i,1]+1],15)
        performance[i,2] ~ Normal(skills[games[i,2]+1],15)

        if games[i,3] == 1
            true ~ Dirac(performance[i,1] > performance[i,2])
        elseif games[i,3] == 0
            true ~ Dirac(performance[i,1] == performance[i,2])
        else 
            true ~ Dirac(performance[i,1] < performance[i,2])
        end
    end

end 

# 0 1 2
players = parse.(Int64,split(readline("true_skill_simple_r2_players.csv"),","))

# 0  1  1
# 1  2  1
# 0  2  1
games = Matrix(CSV.read("true_skill_simple_r2_games.csv",header=false,DataFrame))
println("games:")
display(games)


model = true_skill_simple_r2(players,games)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)

display(chns)
