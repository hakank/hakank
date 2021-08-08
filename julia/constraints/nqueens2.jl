#=

N-Queens problem in ConstraintSolver.jl 

Performance (timeout 60s):
n:8
  0.004049 seconds (23.04 k allocations: 1.663 MiB)

n:12
  0.018140 seconds (113.31 k allocations: 8.275 MiB)

n:50
  0.102909 seconds (531.49 k allocations: 43.128 MiB)

n:100
  0.297124 seconds (637.23 k allocations: 69.590 MiB, 34.21% gc time)

n:200
  1.427308 seconds (2.57 M allocations: 341.693 MiB, 7.26% gc time)

n:300
  4.453372 seconds (5.99 M allocations: 878.029 MiB, 6.68% gc time)

n:400
 10.641826 seconds (11.54 M allocations: 1.767 GiB, 9.06% gc time)

n:500
status:TIME_LIMIT
 60.136410 seconds (110.37 M allocations: 8.428 GiB, 10.85% gc time)
 
 77.461974 seconds (132.85 M allocations: 11.574 GiB, 10.32% gc time, 0.21% compilation time)


Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function nqueens(n=8,print_solutions=true,all_solutions=true,timeout=6)

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
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

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

    @variable(model, 1 <= x[1:n] <= n, Int)

    @constraint(model, x in CS.AllDifferent())    
    @constraint(model, [x[i] + i for i in 1:n] in CS.AllDifferent())
    @constraint(model, [x[i] - i for i in 1:n] in CS.AllDifferent())

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        if print_solutions
            for sol in 1:num_sols
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end
    println("num_sols: $num_sols")
    return status, num_sols
end

@time nqueens(8)

for n in [8,12,50,100,200,300,400,501]
    println("\nn:$n")
    @time status, num_sols = nqueens(n,false,false,60)
    if status == MOI.TIME_LIMIT 
        break
    end
end 
