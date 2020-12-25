#=

  Set covering problem in Julia ConstraintSolver.jl

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering2(problem,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>true,
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

    n = problem[:n]
    corners = problem[:corners]
    num_rows,_ = size(corners)

    # Where to place the telephones
    @variable(model, x[1:n], Bin)
    @variable(model, 1 <= z <= n, Int)

    # All streets must be covered
    for i in 1:num_rows
        @constraint(model,x[corners[i,1]] + x[corners[i,2]] >= 1)
    end
    @constraint(model, sum(x) == z)
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
                println("z:$z_val")
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

problem = Dict(
   :n => 8,
   :corners => resize_matrix([[1,2],
                              [2,3],
                              [4,5],
                              [7,8],
                              [6,7],
                              [2,6],
                              [1,6],
                              [4,7],
                              [2,4],
                              [5,8],
                              [3,5]]),

)
@time set_covering2(problem)
