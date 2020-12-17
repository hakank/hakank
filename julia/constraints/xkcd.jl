#=

  xkcd's knapsack/subset-sum problem in Julia + ConstraintSolver.jl

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total of exact 15.05

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
using Distributions
include("constraints_utils.jl")


function xkcd(prices=[215, 275, 335, 355, 420, 580],total=1505,max_val=10;all_solutions=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            "simplify"=>true,

                                                            "time_limit"=>14,
                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))

    println("prices:$prices")
    println("total:$total   (max_val:$max_val)")
    len = length(prices)

    @variable(model, 0 <= x[1:len] <= max_val, Int)

    # @constraint(model, total == sum([x[i]*prices[i] for i in 1:len]))
    scalar_product(model,x,prices,total)
    # @constraint(model, total == sum(x.*prices))

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")
        # num_sols2 = JuMP.result_count(model) # same result
        # println("num_sols2:$num_sols2\n")

        # The solutions are in reverse order, i.e. the optimal is the first.
        for sol in 1:num_sols
            println("solution #$sol")
            # This works:
            xx = convert.(Integer,JuMP.value.(x,result=sol))
            println("x:$xx")
            println("used: ")
            println([(i,xx[i]) for i in 1:length(xx) if xx[i] > 0  ])
        end
    end
end

xkcd(;all_solutions=true)

println("\nA random instance")

n = 120
prices = rand(DiscreteUniform(100,1000),n)
s = sum(prices)
total = rand(DiscreteUniform(0,s))
max_val = rand(DiscreteUniform(1,100))
xkcd(prices,total,max_val)
