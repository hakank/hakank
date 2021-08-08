#=

  Golomb ruler in Julia ConstraintSolver.jl 

  A Golomb ruler is a set of integers (marks) a(1) < ...  < a(n) such
  that all the differences a(i)-a(j) (i > j) are distinct.  Clearly we
  may assume a(1)=0.  Then a(n) is the length of the Golomb ruler.
  For a given number of marks, n, we are interested in finding the
  shortest Golomb rulers.  Such rulers are called optimal. 

  See http://www.research.ibm.com/people/s/shearer/grule.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function golomb_ruler(n=6,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            # "activity.decay" => 0.999, # default 0.999
                                                            # "activity.max_probes" => 6, # default, 10
                                                            # "activity.max_confidence_deviation" => 10, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=> timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    # m = 2^(n-1) -1 
    m = n^2
    @variable(model, 0 <= x[1:n] <= m, Int)

    @constraint(model, x[1] == 0)
    @constraint(model, x in CS.AllDifferent())
    increasing(model,x)

    
    # This don't work: 
    # """Each variable must be an integer and bounded."""
    # diffs = [x[i]-x[j] for i in 1:n, j in 1:n if i < j]
    # println("diffs: $diffs")
    # This works:
    diffs = [] 
    for i in 1:n, j in 1:n 
        if i != j 
            d = @variable(model,integer=true, lower_bound = -m, upper_bound = m )
            @constraint(model, d == x[i]-x[j])
            push!(diffs,d)
        end
    end
    
    @constraint(model, diffs in CS.AllDifferent())

    # Symmetry breaking
    if n > 2
        @constraint(model, x[2] - x[1] < x[n] - x[n-1] )
    end
    @constraint(model, diffs[1] < diffs[end])

    @objective(model,Min,x[n])

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                diffs_val = convert.(Integer,JuMP.value.(diffs; result=sol))
                println("x:$x_val")
                println("diffs:$diffs_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end # end golomb_ruler




# @time golomb_ruler(8,true,false) # 1.5

# n  time 
# -------
#  8   1.3s
#  9  13.3s
# 10  > 60s
for n in 2:10
    println("\nn:$n")
    @time status = golomb_ruler(n,true,false,60)
    if status == MOI.TIME_LIMIT 
        break
    end
end
