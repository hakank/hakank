#=

  Archery Puzzle in Julia ConstraintSolver.jl 

  http://www.eonhq.com/m/puzzles/images/archery-puzzle.jpg
  Archery puzzle by Sam Loyd:
  """
  How close can the young archer come to scoring a total of
  100 - using as many arrows as she please.
  [The targets are: 16, 17, 23, 24, 39, 40]
  """
  Via: The Aperiodical: "Manchester MathsJam June 2012 Recap"
  http://aperiodical.com/2012/06/manchester-mathsjam-june-2012-recap/


  Solution (unique):
  x = [2,4,0,0,0,0]
  z = 100

  i.e. 2 arrows on score 16 and 4 arrows on score 17:
    2*16 + 4*17 = 100


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function archery_puzzle(print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>all_solutions, 
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

                                                            "time_limit" => 6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    
    n = 6
    targets =  [16, 17, 23, 24, 39, 40]
    target = 100
    @variable(model, 0 <= x[1:n] <= 10, Int)
    @variable(model, 0 <= z <= n*maximum(targets), Int)
    @variable(model, 0 <= diff_z <= n*maximum(targets), Int)

    scalar_product(model,x,targets,:(==),z)
    my_abs(model,target,z,diff_z)


    @objective(model,Min,diff_z)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                diff_z_val = convert.(Integer,JuMP.value.(diff_z; result=sol))
                println("x:$x_val")
                println("z:$z_val diff_z:$diff_z_val")
                println(["target:$(targets[i]) x:$(x_val[i])  " for i in 1:n if x_val[i] > 0]|>join)
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time archery_puzzle(true,true)