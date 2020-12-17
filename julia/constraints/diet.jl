#=

  Diet problem in Julia ConstraintSolver.jl

  Standard diet problem.

  Minimize the cost for the products:
   Type of                        Calories   Chocolate    Sugar    Fat
   Food                                      (ounces)     (ounces) (ounces)
   Chocolate Cake (1 slice)       400           3            2      2
   Chocolate ice cream (1 scoop)  200           2            2      4
   Cola (1 bottle)                150           0            4      1
   Pineapple cheesecake (1 piece) 500           0            4      5


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function diet(problem,print_solutions=true,all_solutions=true)

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

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    prices = problem[:prices]
    limits = problem[:limits]
    nutrition = problem[:nutrition]
    max_val = problem[:max_val]

    len = length(prices)

    @variable(model, 0 <= x[1:len] <= max_val, Int)
    @variable(model, 1 <= z <= maximum(prices)*max_val, Int)


    for i in 1:len
        scalar_product(model,nutrition[i,:],x,:(>=),limits[i])
    end

    @constraint(model, z == sum(prices.*x) )

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
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

diet_problems = Dict(
:1 => Dict(
     :prices => [ 50, 20, 30, 80.4],  # price in cents for each nutrition
     :limits => [500,  6, 10,  8], #limits, requirements for each nutrition type

     # nutrition for each product
     :nutrition =>
     resize_matrix([[400, 200, 150, 500],  # calories
                  [  3,   2,   0,   0],  # chocolate
                  [  2,   2,   4,   4],  # sugar
                  [  2,   4,   1,   5]] # fat
   ),
     :max_val => 10
   ),
)

@time diet(diet_problems[:1])
