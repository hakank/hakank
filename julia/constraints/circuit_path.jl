#=

  circuit_path in Julia ConstraintSolver.jl

  circuit_path is the same as circuit(model, x)
  where x is the cicuit. Here we also expose the
  z array which containts the path.

  The advantage of this is that one can impose
  constraints on the path as well as on the circuit.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# The circuit_path() constraint is in constraints_utils.jl
#

#
# The number of solutions for n is (n-1)!
#
# The following is almost identical as circuit_test in circuit.jl
#
function circuit_path_test(n,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                        ))

    @variable(model, 1 <= x[1:n] <= n, Int)
    @variable(model, 1 <= path[1:n] <= n, Int)
    
    circuit_path(model, x, path)

    # Example of constraint on path
    # @constraint(model, path[1] == 2)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                pathx = convert.(Integer,JuMP.value.(path; result=sol))
                println("x   :$xx")
                println("path:$pathx")
                println()

            end
        end
        println("num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status
end

function circuit_path_test_range(from, to)
    print_solutions=true
    all_solutions=true
    for n in from:to

        println("n:$n")
        if n > 5
            print_solutions = false
        end
        if n > 8
            all_solutions = false
            print_solutions = true
        end
        @time status = circuit_path_test(n,print_solutions,all_solutions)
        if status != MOI.OPTIMAL
            break
        end
    end
end

circuit_path_test_range(2,39)
