#=
  Set covering in Julia ConstraintSolver.jl

  Placing of firestations, from Winston "Operations Research", page 486

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering()

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))
    min_distance = 15 # minimum distance
    # distances between the cities
    distance = [[0,10,20,30,30,20],
               [10,0,25,35,20,10],
               [20,25,0,15,30,20],
               [30,35,15,0,15,25],
               [30,20,30,15,0,14],
               [20,10,20,25,14,0]]
    num_cities = length(distance)

    # where to place the fire stations: 1 if placed in this city.
    @variable(model, x[1:num_cities], Bin)
    @variable(model, 0 <= z <= num_cities, Int)

    # calculate the number of covered fire stations
    for j in 1:num_cities
        @constraint(model,sum([x[i] for i in 1:num_cities if distance[i][j] <= min_distance]) >= 1)
    end

    # objective: minimize the number of fire stations
    @constraint(model, z == sum(x))

    @objective(model, Min, z)

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
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                zz = convert.(Integer,JuMP.value.(z; result=sol))
                println("x:$xx z:$zz")

            end
        end
    else
        println("status:$status")
    end

    return status
end

@time set_covering()
