#=

  P-median problem in ConstraintSolver.jl 

  Model and data from the OPL Manual, which describes the problem:
  """
  The P-Median problem is a well known problem in Operations Research. 
  The problem can be stated very simply, like this: given a set of customers 
  with known amounts of demand, a set of candidate locations for warehouses, 
  and the distance between each pair of customer-warehouse, choose P 
  warehouses to open that minimize the demand-weighted distance of serving 
  all customers from those P warehouses.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function p_median(problem, print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
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
    demand = problem[:demand]
    distance = problem[:distance]
    p = problem[:p]
    num_customers,num_warehouses = size(distance)
    
    println("p:$p")
    println("demand:$demand")
    println("distance:$distance")
    println("num_warehouses:$num_warehouses num_customers:$num_customers")

    @variable(model, open_warehouse[1:num_warehouses], Bin)
    @variable(model, ship_to_customer[1:num_customers,1:num_warehouses], Bin)
    @variable(model, 0 <= z <= sum(demand)*num_customers, Int)

    @constraint(model, z == sum(demand[c]*distance[c,w]*ship_to_customer[c,w] 
                            for c in 1:num_customers, w in 1:num_warehouses))

    for c in 1:num_customers
        @constraint(model, sum([ship_to_customer[c,w] for w in 1:num_warehouses]) == 1)
    end

    @constraint(model, p == sum(open_warehouse))

    for c in 1:num_customers, w in 1:num_warehouses 
        @constraint(model, ship_to_customer[c,w] <= open_warehouse[w])
    end


    @objective(model,Min,z)

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
                open_warehouse_val = convert.(Integer,JuMP.value.(open_warehouse; result=sol))
                ship_to_customer_val = convert.(Integer,JuMP.value.(ship_to_customer; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("open_warehouse:$open_warehouse_val")
                println("ship_to_customer:$ship_to_customer_val")
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
    :demand => [100,80,80,70],
    :distance => resize_matrix([[2, 10, 50],
                                [2, 10, 52],
                                [50, 60,  3],
                                [40, 60,  1]]),
    :p => 2,
)
@time p_median(problem)
