#=

  (Decomposition of) global constraint circuit in Julia ConstraintSolver.jl

  See Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# The circuit() constraint is in constraints_utils.jl
#

#
# The number of solutions for n is (n-1)!
#
function circuit_test(n,print_solutions=true,all_solutions=true)

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
    circuit(model, x)

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
                println("x:$xx")

            end
        end
        println("num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status
end

function circuit_test_range(from, to)
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
        @time status = circuit_test(n,print_solutions,all_solutions)
        if status != MOI.OPTIMAL
            break
        end
    end
end

circuit_test_range(2,39)
