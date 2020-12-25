#=

  Set covering in Julia.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such 
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering_skiena(problem,print_solutions=true,all_solutions=true)

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

    n, m = size(problem)

    @variable(model, x[1:n], Bin)
    @variable(model, 1 <= z <= n, Int)

    for b in eachrow(transpose(problem))
        @constraint(model, sum(x.*b) >= 1)
    end
    @constraint(model,z == sum(x))

    @objective(model,Min,z)

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
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("x:$x_val")
                println("z:$z_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

set_covering_skiena_problems = Dict(
:1 => resize_matrix([[1,1,0,0,0,0,0,0,0,0,0,0],
                    [0,1,0,0,0,0,0,1,0,0,0,0],
                    [0,0,0,0,1,1,0,0,0,0,0,0],
                    [0,0,0,0,0,1,1,0,0,1,1,0],
                    [0,0,0,0,0,0,0,0,1,1,0,0],
                    [1,1,1,0,1,0,0,0,1,1,1,0],
                    [0,0,1,1,0,0,1,1,0,0,1,1]])
)


@time set_covering_skiena(set_covering_skiena_problems[:1])

