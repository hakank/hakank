#=

  Zebra puzzle in Julia ConstraintSolver.jl

  Lewis Carrol's classical puzzle with five houses and a zebra:
  
  Five men with different nationalities live in the first five houses
  of a street.  They practise five distinct professions, and each of
  them has a favourite animal and a favourite drink, all of them
  different.  The five houses are painted in different colours.
  
  The Englishman lives in a red house.
  The Spaniard owns a dog.
  The Japanese is a painter.
  The Italian drinks tea.
  The Norwegian lives in the first house on the left.
  The owner of the green house drinks coffee.
  The green house is on the right of the white one.
  The sculptor breeds snails.
  The diplomat lives in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian's house is next to the blue one.
  The violinist drinks fruit juice.
  The fox is in a house next to that of the doctor.
  The horse is in a house next to that of the diplomat.
  
  Who owns a Zebra, and who drinks water?
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function zebra(print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            # "activity.decay" => 0.999, # default 0.999
                                                            # "activity.max_probes" => 10, # default, 10
                                                            # "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n = 5
    @variable(model, 1 <= nationality[1:n] <= n, Int)
    English, Spaniard, Japanese, Italian, Norwegian = nationality
    nationality_str = ["English", "Spaniard", "Japanese", "Italian", "Norwegian"]
    @variable(model, 1 <= color[1:n] <= n, Int)
    Red, Green, White, Yellow, Blue = color
    @variable(model, 1 <= profession[1:n] <= n, Int)
    Painter, Sculptor, Diplomat, Violinist, Doctor = profession
    @variable(model, 1 <= pet[1:n] <= n, Int)
    Dog, Snails, Fox, Horse, Zebra = pet
    @variable(model, 1 <= drink[1:n] <= n, Int)
    Tea, Coffee, Milk, Juice, Water = drink

    @constraint(model, nationality in CS.AllDifferentSet())
    @constraint(model, color in CS.AllDifferentSet())
    @constraint(model, profession in CS.AllDifferentSet())
    @constraint(model, pet in CS.AllDifferentSet())
    @constraint(model, drink in CS.AllDifferentSet())

    @constraint(model, English == Red)
    @constraint(model, Spaniard == Dog)
    @constraint(model, Japanese == Painter)
    @constraint(model, Italian == Tea)
    @constraint(model, Norwegian == 1)
    @constraint(model, Green == Coffee)
    @constraint(model, Green == White + 1)
    @constraint(model, Sculptor == Snails)
    @constraint(model, Diplomat == Yellow)
    @constraint(model, Milk == 3)
    my_abs(model,Norwegian,Blue,1)
    @constraint(model, Violinist == Juice)
    my_abs(model,Fox,Doctor,1)
    my_abs(model, Horse,Diplomat, 1)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                nationality_val = convert.(Integer,JuMP.value.(nationality; result=sol))
                color_val = convert.(Integer,JuMP.value.(color; result=sol))
                profession_val = convert.(Integer,JuMP.value.(profession; result=sol))
                pet_val = convert.(Integer,JuMP.value.(pet; result=sol))
                drink_val = convert.(Integer,JuMP.value.(drink; result=sol))
                println("nationality:$nationality_val")
                println("color      :$color_val")
                println("profession :$profession_val")
                println("pet        :$pet_val")
                println("drink      :$drink_val")
                zebra_val = convert.(Integer,JuMP.value.(Zebra; result=sol))
                water_val = convert.(Integer,JuMP.value.(Water; result=sol))
                println("zebra:$zebra_val water:$water_val")
                z = ["$(nationality_str[i]) owns the zebra\n" for i in 1:n if nationality_val[i] == zebra_val][1]
                w = ["$(nationality_str[i]) drinks water\n" for i in 1:n if nationality_val[i] == water_val][1]
                println("$z$w")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time zebra()
