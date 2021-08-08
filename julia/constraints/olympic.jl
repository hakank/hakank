#=

Olympic puzzle in Julia ConstraintSolver.jl 

Benchmark for Prolog (BProlog)
"""
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993

  Purpose: solve a puzzle taken from Olympic Arithmetic Contest

   Given ten variables with the following configuration:

               X7   X8   X9   X10

                  X4   X5   X6

                     X2   X3             

                        X1

  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables 
                      Xi   Xj

                         Xk
  the following constraint is satisfied:

                    |Xi-Xj| = Xk
"""



Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Printf
const CS = ConstraintSolver
include("constraints_utils.jl")

function olympic(print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n = 10
    @variable(model, 1 <= x[1:n] <= n, Int)
    x1,x2,x3,x4,x5,x6,x7,x8,x9,x10 = x
    @constraint(model, x in CS.AllDifferent())

    @constraint(model,x1 == 3)
    my_abs(model,x2,x3,x1)
    my_abs(model,x4,x5,x2)
    my_abs(model,x5,x6,x3)
    my_abs(model,x7,x8,x4)
    my_abs(model,x8,x9,x5)
    my_abs(model,x9,x10,x6)


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
                # println("x:$xx")
                @printf "%d  %d  %d %d\n"  xx[7] xx[8] xx[9] xx[10]
                @printf "  %d  %d  %d\n" xx[4] xx[5] xx[6]
                @printf "    %d  %d\n" xx[2] xx[3]
                @printf "      %d\n" xx[1]
                println()
             
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time olympic()
