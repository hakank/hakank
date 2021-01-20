#=

  Simple PERT model in ConstraintSolver.jl 

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function pert(problem, print_solutions=true,all_solutions=true,timeout=6)

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

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    times        = problem[:times]
    dependencies = problem[:dependencies]
    max_time     = problem[:max_time]
    n = length(times)

    @variable(model, 0 <= start[1:n] <= max_time, Int)
    @variable(model, 0 <= s_end <= max_time, Int)

    for (d1,d2) in eachrow(dependencies)
        @constraint(model, start[d1] >= start[d2]+times[d2])
    end

    @constraint(model, s_end == start[n]) # to minimize
 
    @objective(model,Min,s_end)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                start_val = convert.(Integer,JuMP.value.(start; result=sol))
                s_end_val = convert.(Integer,JuMP.value.(s_end; result=sol))
                println("start:$start_val")
                println("s_end:$s_end_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

problem = Dict(
             # Note: There is no i
             # a  b  c  d  e  f  g  h  j  k  Send 
    :times => [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1],
    # Dependencies
    :dependencies => resize_matrix(
            [[2,1],  # Sb >= Sa + 7
             [4,1],  # Sd >= Sa + 7
             [3,2],  # Sc >= Sb + 3
             [5,3],  # Se >= Sc + 1
             [5,4],  # Se >= Sd + 8
             [7,3],  # Sg >= Sc + 1
             [7,4],  # Sg >= Sd + 8
             [6,4],  # Sf >= Sd + 8
             [6,3],  # Sf >= Sc + 1
             [8,6],  # Sh >= Sf + 1
             [9,8],  # Sj >= Sh + 3
             [10,7], # Sk >= Sg + 1
             [10,5], # Sk >= Se + 1
             [10,9], # Sk >= Sj + 2
             [11,10] # Send >= Sk + 1
            ]),
    :max_time => 30,
)

@time pert(problem)
