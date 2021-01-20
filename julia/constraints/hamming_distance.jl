#=

  Hamming distance in ConstraintSolver.jl 
 
  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function hamming_distance(a,print_solutions=true,all_solutions=true,timeout=6)

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
    n = length(a)
    println("a:$a n:$n")
    @variable(model, x[1:n], Bin)
    @variable(model, 0 <= diffs <= n, Int)

    # This don't work in the current version (v0.6.3)
    # @constraint(model, diffs == sum(x .!= a))
    # So we have to rewrite it...

    count_ctr2(model,x, :(!=), a, diffs)
    #=
    @variable(model, b[1:n], Bin)
    for i in 1:n 
        @constraint(model, b[i] := {x[i] != a[i]})
    end        
    @constraint(model, diffs == sum(b))
    =#

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                diffs_val = convert.(Integer,JuMP.value.(diffs; result=sol))
                println("x:$x_val diffs:$diffs_val")

            end
        end
    else
        println("status:$status")
    end
    println("num_sols:$num_sols\n")
    return status, num_sols
end

a = ones(Int,4)
@time hamming_distance(a)
