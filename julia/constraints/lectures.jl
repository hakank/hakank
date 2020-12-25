#=

  Lectures problem in Julia ConstraintSolver.js

  Biggs: Discrete Mathematics (2nd ed), page 187.
  """   
  Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
  Among the the potential audience there are people who wish to hear both
 
   - v1 and v2
   - v1 and v4
   - v3 and v5
   - v2 and v6
   - v4 and v5
   - v5 and v6
   - v1 and v6
 
  How many hours are necessary in order that the lectures can be given
  without clashes?
  """    

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function lectures(problem,print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    n, _ = size(problem)
    @variable(model, 1 <= x[1:n] <= n, Int)
    @variable(model, 1 <= max_t <= n, Int)

    for (l1,l2) in eachrow(problem)
        @constraint(model, x[l1] != x[l2])
    end

    # symmetry breaking
    for i in 1:n
        @constraint(model, x[i] <= i)
    end
    my_max(model,x,max_t)
    @objective(model,Min,max_t)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                max_t_val = convert.(Integer,JuMP.value.(max_t; result=sol))
                println("x:$x_val")
                println("max_t:$max_t_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

# The schedule requirements:
#   lecture a cannot be held at the same time as b
 lectures_problem = resize_matrix([[1, 2],
                                    [1, 4],
                                    [3, 5],
                                    [2, 6],
                                    [4, 5],
                                    [5, 6],
                                    [1, 6]])

@time lectures(lectures_problem)
