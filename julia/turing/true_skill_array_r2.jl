#=

    This is a port of the R2 model TrueSkillArray.cs

    Output from the R2 model (skills):
    ```
    [0] Mean: 104.314
    [0] Variance: 71.6896
    [1] Mean: 99.9344
    [1] Variance: 71.7215
    [2] Mean: 92.4195
    [2] Variance: 63.2211
    ```

    Note: The only difference between R2's model TrueSkill_Simple.cs and TrueSkillArray.cs
    is that TrueSkill_Simple.cs get the data from files while TrueSkillArray.cs get the 
    data from arrays in the file. The basic models are the same as well as the data
    (3 players and 3 matches).

    Here I simulate a little larger example: 10 players and 20 matches between random players.

    Players: 1:10
    Skills: [101.86991042639397, 99.81168650944693, 84.17600281171374, 99.55322670368798, 91.70866945610942, 99.41376712574124, 90.1144607234882, 96.8904551041217, 116.32792430446636, 103.5346117316912]
    Ranks based on skills (decreasing): [9, 10, 1, 2, 4, 6, 8, 5, 7, 3]
    Games:
    Game 1: [1, 10, 1]
    Game 2: [9, 3, 1]
    Game 3: [3, 2, -1]
    Game 4: [1, 7, -1]
    Game 5: [8, 3, 1]
    Game 6: [5, 6, -1]
    Game 7: [6, 9, -1]
    Game 8: [10, 3, 1]
    Game 9: [4, 2, -1]
    Game 10: [10, 5, 1]
    Game 11: [9, 2, -1]
    Game 12: [6, 2, 1]
    Game 13: [6, 1, 1]
    Game 14: [5, 1, -1]
    Game 15: [10, 6, -1]
    Game 16: [10, 3, 1]
    Game 17: [1, 9, 1]
    Game 18: [3, 2, -1]
    Game 19: [10, 3, 1]
    Game 20: [5, 10, -1]

    Summary Statistics
        parameters       mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol    Float64   Float64    Float64   Float64    Float64   Float64       Float64 

         skills[1]    97.1878    1.0445     0.0330    0.3302     2.1884    3.3300        0.3355
         skills[2]   105.1533    9.6393     0.3048    3.0467     2.1886    3.3293        0.3356
         skills[3]    95.1222    6.0911     0.1926    1.9118     2.2049    3.2117        0.3381
         skills[4]   102.8225    7.4180     0.2346    2.3437     2.1894    3.3231        0.3357
         skills[5]    97.1789    0.4437     0.0140    0.0224   137.7141    1.0058       21.1153
         skills[6]   100.7338    3.2006     0.1012    0.9924     2.2375    3.0112        0.3431
         skills[7]   101.0100    2.4683     0.0781    0.7741     2.2068    3.1994        0.3384
         skills[8]    96.2084    1.0064     0.0318    0.3153     2.2096    3.1808        0.3388
         skills[9]    97.4989    7.2633     0.2297    2.2818     2.2043    3.2137        0.3380
         ⋮              ⋮          ⋮         ⋮          ⋮         ⋮          ⋮           ⋮
                                                                                  41 rows omitted


    Skills (based on performance)
    Player 1: 97.1878066434855
    Player 2: 105.15333131925922
    Player 3: 95.12224324765661
    Player 4: 102.82248436052862
    Player 5: 97.17893913871123
    Player 6: 100.73384589924844
    Player 7: 101.01001164175119
    Player 8: 96.20842558248593
    Player 9: 97.49887408357466
    Player 10: 88.48929120951159

    Sorted ranks (decreasing order):
    Rank 1 Player 2
    Rank 2 Player 4
    Rank 3 Player 7
    Rank 4 Player 6
    Rank 5 Player 9
    Rank 6 Player 1
    Rank 7 Player 5
    Rank 8 Player 8
    Rank 9 Player 3
    Rank 10 Player 10


    Cf true_skill.jl

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV, DataFrames
include("jl_utils.jl")


#
# Simulate skills and games
#
function trueskill_simulate(num_players=5,num_matches=10,skill_mean=100,skill_variance=10,performance_variance=15)
    # This is the real skill for the player
    skills = tzeros(num_players)
    for i in 1:num_players 
        skills[i] = rand(Normal(skill_mean,skill_variance))
    end

    games = []
    # Generate <num_matches> random games.
    match = 0
    while match < num_matches
        p1 = rand(1:num_players)
        p2 = rand(1:num_players)    
        if p1 != p2 
            performance_p1 = rand(Normal(skills[p1],performance_variance))
            performance_p2 = rand(Normal(skills[p2],performance_variance))
            if performance_p1 > performance_p2 
                game = [p1,p2,1]
            elseif performance_p1 == performance_p2 
                game = [p1,p2,0]
            else
                game = [p1,p2,-1]
            end
            push!(games,game)
            match += 1
        end
    end 
    
    return 1:num_players, skills, hcat(games...)'
end


@model function true_skill_array_r2(players,games) 
    num_players = length(players)
    num_games, m = size(games)
    skills ~ filldist(Normal(100,10),num_players)

    performance = Matrix{Real}(undef, num_games,2) 
    for i in 1:num_games 
        performance[i,1] ~ Normal(skills[games[i,1]],15)
        performance[i,2] ~ Normal(skills[games[i,2]],15)

        if games[i,3] == 1
            true ~ Dirac(performance[i,1] > performance[i,2])
        elseif games[i,3] == 0
            true ~ Dirac(performance[i,1] == performance[i,2])
        else 
            true ~ Dirac(performance[i,1] < performance[i,2])
        end
    end

end 


num_players = 10
num_matches = 20
skill_mean = 100
skill_variance = 10 
performance_variance = 15
players, skills,games = trueskill_simulate(num_players,num_matches,skill_mean,skill_variance, performance_variance)
println("Players: $players")
println("Skills: $skills")
println("Ranks based on skills (decreasing): $(reverse(sortperm(skills)))")
println("Games:")
for i in 1:size(games)[1]
    println("Game $i: $(games[i,:])")
end

model = true_skill_array_r2(players,games)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)

display(chns)

println("\nSkills (based on performance)")
game_skills  = mean(group(chns,:skills))[:,2]
for i in 1:num_players
    println("Player $i: $(game_skills[i])")
end
game_ranks = game_skills |> sortperm |> reverse
println("\nSorted ranks (decreasing order):")
for i in 1:num_players
    println("Rank $i Player $(game_ranks[i])")
end
