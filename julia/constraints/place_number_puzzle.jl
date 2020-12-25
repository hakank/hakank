#=

  Place number puzzle in Julia.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function place_number_puzzle(problem,print_solutions=true,all_solutions=true)

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

    graph = problem[:graph]
    rows,cols = size(graph)
    n     = problem[:n]


    @variable(model, 1 <= x[1:n] <= n, Int)

    @constraint(model, x in CS.AllDifferentSet())

    #=
    for i in 1:rows
        # Here we constraint the domain of t (the difference)
        # to >= 1 (i.e. 2..n)
        t = @variable(model, [1:1], CS.Integers(2:n))
        my_abs(model,x[graph[i,1]],x[graph[i,2]],t[1])
    end
    =#
    # Alternative:
    for (i,j) in eachrow(graph)
        t = @variable(model, [1:1], CS.Integers(2:n)) 
        my_abs(model,x[i],x[j],t[1])
    end
    # symmetry breaking
    @constraint(model,x[1] <= x[n])

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

problem = Dict( 
   :graph => resize_matrix([[1,3], [1,4], [1,5],
                            [2,4], [2,5], [2,6],
                            [3,1], [3,4], [3,7],
                            [4,1], [4,2], [4,3], [4,5], [4,7], [4,8],
                            [5,1], [5,2], [5,4], [5,6], [5,7], [5,8],
                            [6,2], [6,5], [6,8],
                            [7,3], [7,4], [7,5],
                            [8,4], [8,5], [8,6]]),
    :n => 8
)



@time place_number_puzzle(problem)
