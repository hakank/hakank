#=

  Magic squares in Julia ConstraintSolver.jl

  This models solves n=3:7 easily but it has problem from n=8

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function magic_square(n=4,print_solutions=true,all_solutions=true)

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
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            # "branch_strategy" => :IMPS, # default
                                                            "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 10, # default, 10
                                                            "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            "time_limit"=> 6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    s = round(Int,n*(n^2 + 1) / 2)
    ss = s
    println("n:$n n*n:$(n^2) s:$s")
    @variable(model, 1 <= x[1:n,1:n] <= n^2, Int)
    # @variable(model, ss <= s <= ss, Int) # It works if s is a variable

    @constraint(model, x[:] in CS.AllDifferent())

    # rows and cols
    for i in 1:n
        # Rows
        # @constraint(model, s == sum(x[i,:]))  # This works
        # The following yield INFEASIBLE
        # I reported this in https://github.com/Wikunia/ConstraintSolver.jl/issues/226
        # This was fixed in ConstraintSolver.jl v0.6.0.
        @constraint(model, sum(x[i,:]) == s)

        # Columns
        # @constraint(model, sum(x[:,i]) == s)
        @constraint(model, s == sum(x[:,i]))
    end

    # diagonals
    # @constraint(model, s == sum([x[i,i] for i in 1:n]))
    @constraint(model, sum([x[i,i] for i in 1:n]) == s)

    # @constraint(model, s == sum([x[i,n-i+1] for i in 1:n]))
    @constraint(model, sum([x[i,n-i+1] for i in 1:n]) == s)

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
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                println("s:$s")
                display(xx)

            end
        end
    else
        println("status:$status")
    end

    return status
end

print_solutions=true
all_solutions=false
for n in 3:10  # 8.. probably time out
    println("\nn:$n")
    @time status = magic_square(n,print_solutions,all_solutions)
    if status != MOI.OPTIMAL
        break
    end
end
