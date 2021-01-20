#=

  Maximum flow problem in ConstraintSolver.jl 

  From Winston "Operations Research", page 420f, 423f
  Sunco Oil example.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function max_flow_winston1(problem, print_solutions=true,all_solutions=true,timeout=6)

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

    cap  = problem[:cap]
    arcs = problem[:arcs]
    n    = problem[:n]
    nodes = problem[:nodes]
    max_val = maximum(cap)
    num_arcs = length(cap)


    @variable(model, 0 <= x[1:n,1:n] <= max_val, Int)
    @variable(model, 0 <= z <= max_val*n, Int)

    @constraint(model, z == x[n,1])
    for i in 1:num_arcs 
        @constraint(model,x[arcs[i,1], arcs[i,2]] <= cap[i])
    end 
    for i in nodes
        @constraint(model,
          sum([x[arcs[k,1], arcs[k,2]] for k in 1:num_arcs if arcs[k,1] == i])
          ==
          sum([x[arcs[k,1], arcs[k,2]] for k in 1:num_arcs if arcs[k,2] == i])
        )
    end
 
    @objective(model,Max,z)

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
                println("x:")
                for row in eachrow(x_val)
                    println(row)
                end
                println("z:$z_val")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

problem = Dict( 
    :cap => [2,3,3,4,2,1,100],
    :arcs =>  resize_matrix([[1, 2],
                            [1, 3],
                            [2, 3],
                            [2, 4],
                            [3, 5],
                            [4, 5],
                            [5, 1]]),
    :n => 5,
    :nodes => 1:5,
)

@time max_flow_winston1(problem,true,false)
