#=
    Least diff problem in Julia + ConstraintSolver.jl

    The model solves the following problem:

    What is the smallest difference between two numbers X - Y
    if you must use all the digits (0..9) exactly once, i.e.
    Minimize the difference
        ABCDE - FGHIJ

    x:[5, 0, 1, 2, 3, 4, 9, 8, 7, 6]
    247 = 50123 - 49876
    0.603865 seconds (833.03 k allocations: 1.107 GiB, 7.89% gc time)

    Note: using diff as a separate variable is much slower.


=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# 0.603865s
#
function least_diff()

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> true,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf, # Best

                                                            # "simplify"=>false,
                                                            "simplify"=>true,

                                                            "time_limit"=>10,

                                                            # "lp_optimizer" => cbc_optimizer, # much slower (4.7s)
                                                            # "lp_optimizer" => glpk_optimizer, # (0.7s)
                                                            # "lp_optimizer" => ipopt_optimizer, # strange result!
                                        ))

    @variable(model, 0 <= x[1:10] <= 9, Int)
    @variable(model, 10000 <= v[1:2] <= 99999, Int)
    # @variable(model, 0 <= diff <= 999, Int) # much slower

    a,b,c,d,e,f,g,h,i,j = x
    @constraint(model, x in CS.AllDifferent())
    @constraint(model, v[1] == 10000*a + 1000*b + 100*c + 10*d + e)
    @constraint(model, v[2] == 10000*f + 1000*g + 100*h + 10*i + j)

    # @constraint(model, diff == v[1] - v[2])  # much slower
    # @constraint(model, diff >= 1)
    @constraint(model, v[1] - v[2] >= 1)

    # @constraint(model, diff == 247) # Testing
    # @constraint(model, v[1] - v[2] == 247) # Testing

    # @objective(model, Min, diff)
    # This is much faster than working with a variable `diff`
    @objective(model, Min, v[1]-v[2])

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        x = convert.(Integer,JuMP.value.(x))
        abcde = convert.(Integer,JuMP.value.(v[1]))
        fghij = convert.(Integer,JuMP.value.(v[2]))
        # diff = convert.(Integer,JuMP.value.(v[1]-v[2]))
        diff = abcde - fghij
        # diff = convert.(Integer,JuMP.value.(diff))

        println("x:$x")
        println("$diff = $abcde - $fghij")

        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")

        # The solutions are in reverse order, i.e. the optimal is the first.
        for sol in num_sols:-1:1
            println("solution #$sol")
            # This works:
            vv = convert.(Integer,JuMP.value.(v,result=sol))
            println("vv:$vv diff: $(vv[1]-vv[2]) $(sol==1 ? """optimal!""" : "")")
        end
    end
end

least_diff()
