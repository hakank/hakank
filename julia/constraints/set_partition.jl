#=

  Set partition problem in Julia ConstraintSolver.jl

  Problem formulation from
    http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n},
   it consists in finding two sets A and B such that:

    *  A U B = S
    *  |A| = |B|
    * sum(A) = sum(B)
    * sum_squares(A) = sum_squares(B)
  """


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_partition(n=8,num_sets=2,print_solutions=true,all_solutions=true)

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
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            # "branch_strategy" => :IMPS, # default
                                                            "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 100, # default, 10
                                                            "activity.max_confidence_deviation" => 1, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    println("n:$n num_sets:$num_sets")
    n2 = round(Int, n / num_sets)
    if n % 4 != 0
        println("n ($n) must be a multiple of 4")
        return
    end

    # Which set (1..num_sets) is  1..n assigned to
    @variable(model, 1 <= a[1:n] <= num_sets, Int)

    # number of integers in each set
    @variable(model, n2 <= counts[1:num_sets] <= n2, Int)

    @variable(model, 0 <= sums[1:num_sets] <= n*n, Int)
    @variable(model, 0 <= sum_squared[1:num_sets] <= n*n*n, Int)

    for s in 1:num_sets
        count_ctr(model, a, :(==), s, counts[s])
        b = @variable(model, [1:n],Bin)
        for i in 1:n
            @constraint(model, b[i] := {a[i] == s})
        end
        @constraint(model,sums[s] == sum(i*b[i] for i in 1:n))
        @constraint(model,sum_squared[s] == sum(i*i*b[i] for i in 1:n))
    end

    for s in 1:num_sets-1
        @constraint(model,sums[s] == sums[s+1])
        @constraint(model,sum_squared[s] == sum_squared[s+1])
    end

    # Symmetry breaking
    @constraint(model,a[1] == 1)

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
                a_val = convert.(Integer,JuMP.value.(a; result=sol))
                sums_val = convert.(Integer,JuMP.value.(sums; result=sol))
                sum_squared_val = convert.(Integer,JuMP.value.(sum_squared; result=sol))
                counts_val = convert.(Integer,JuMP.value.(counts; result=sol))
                println("a:$a_val")
                for s in 1:num_sets
                    println("set $s: ", [i for i in 1:n if a_val[i] == s])
                end
                println("sums:$sums_val")
                println("sum_squared:$sum_squared_val")
                println("counts:$counts_val")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

# @time set_partition(16,2,true,false)
for i in 2:10
    n = 4*i
    println("\nn:$n")
    @time status = set_partition(n,2,true,false)
    if status != MOI.OPTIMAL
        break
    end
end
