#=
  Assignment problems in Julia ConstraintSolver.jl

  Different assignments problem, both minimization and maximization.
  See the sources of the problem below.

  Compare to the following MiniZinc models, from which these problems
  are taken:
  * http://www.hakank.org/minizinc/assignment.mzn
  * http://www.hakank.org/minizinc/assignment2.mzn
  * http://www.hakank.org/minizinc/assignment2_2.mzn
  * http://www.hakank.org/minizinc/assignment3.mzn
  * http://www.hakank.org/minizinc/assignment5.mzn
  * http://www.hakank.org/minizinc/assignment6.mzn


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")
include("assignment_instances.jl")

function assignment(data)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> true,
                                                            "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                        ))

    op = data[:op]
    cost  = resize_matrix(data[:m])
    rows, cols = size(cost)

    @variable(model, x[1:rows,1:cols], Bin)
    @variable(model, 0 <= total_cost <= sum(cost), Int)

    # exacly one assignment per row, all rows must be assigned
    for i in 1:rows
        @constraint(model,sum([x[i,j] for j in 1:cols]) == 1 )
    end

    # zero or one assignments per column
    for j in 1:cols
        @constraint(model,sum([x[i,j] for i in 1:rows]) <= 1)
    end

    # calculate TotalCost

    @constraint(model,total_cost == sum(x.*cost))

    # optimization mode
    if op == :minimize
       @objective(model,Min,total_cost)
    else
        @objective(model,Max,total_cost)
    end

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
                total_costx = convert.(Integer,JuMP.value.(total_cost; result=sol))
                println("total_cost:$total_costx")
                assignments = [j for i in 1:rows for j in 1:cols if xx[i,j] == 1]
                println("Assignments: $assignments")
                #=
                for row in eachrow(xx)
                    println(row)
                end
                =#
            end
        end
    else
        println("status:$status")
    end

    return status
end


for instance in sort(collect(keys(assignment_instances)))
    println("\ninstance: $instance")
    @time assignment(assignment_instances[instance])
end
