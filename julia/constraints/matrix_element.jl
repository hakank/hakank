#=
    Decomposition of global constraint matrix_element in Julia ConstraintSolver.jl

    matrix_element(model,x,i,j,val)

    ensures that  x[i,j] = val

    Model created by Hakan Kjellerstrand, hakank@gmail.com
    See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


#
# matrix_element is in constraints_utils.jl
#
function matrix_element_test(n=4,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            "branch_split"=>:Biggest, # fastest for this simple model!
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>3,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    m = n
    @variable(model, 1 <= x[1:n,1:m] <= n, Int)

    # @constraint(model, x[:] in CS.AllDifferent())
    @variable(model, 1 <= ii <= n, Int)
    @variable(model, 1 <= jj <= m, Int)
    @variable(model, 1 <= val <= n, Int)

    for i in 1:n
        @constraint(model, x[i,:] in CS.AllDifferent())
        @constraint(model, x[:,i] in CS.AllDifferent())
    end

    @constraint(model, val == n)
    # @constraint(model, ii == 2)
    # @constraint(model, jj == 3)

    matrix_element(model,x,ii,jj,val)

    # @objective(model,Min,x[n,m])

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
                iix = convert.(Integer,JuMP.value.(ii; result=sol))
                jjx = convert.(Integer,JuMP.value.(jj; result=sol))
                valx = convert.(Integer,JuMP.value.(val; result=sol))

                # print_grid(xx)
                display(xx)
                println("ii:$iix jj:$jjx val:$valx")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

print_solutions=true
all_solutions=false
for n in 2:20
    println("\nn:$n")
    @time status = matrix_element_test(n,print_solutions,all_solutions)
    if status != MOI.OPTIMAL
        break
    end
end
