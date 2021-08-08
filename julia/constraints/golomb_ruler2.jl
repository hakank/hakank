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


#
# Alternativ approach:
# Inspired by 
# Barbara M. Smith, Kostas Stergiou, and Toby Walsh:
# "Modelling the Golomb Ruler Problem"
#
function golomb_ruler2(n=6,print_solutions=true,all_solutions=true)

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

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            # "activity.decay" => 0.999, # default 0.999
                                                            # "activity.max_probes" => 6, # default, 10
                                                            # "activity.max_confidence_deviation" => 10, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=> 6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    m = n^2
    @variable(model, 0 <= x[1:n] <= m, Int)

    @constraint(model, 0 == x[1])

    for i in 2:n
        @constraint(model, x[i-1] <= x[i])
        @constraint(model, x[i-1] != x[i])
    end
    increasing_strict(model, x)


    for i in 1:n, j in i+1:n, k in j+1:n
        @constraint(model, 2*x[j] - x[i] - x[k] != 0 )
    end
    
    for i in 1:n, j in 1:n, k in 1:n, l in 1:n 
        if i < j && k < l && [i,j] < [k,l]
            @constraint(model, (x[j] - x[i]) != (x[l] - x[k]))

            # This works
            # t1 = @variable(model, integer=true, lower_bound=-m, upper_bound=m)
            # t2 = @variable(model, integer=true, lower_bound=-m, upper_bound=m)
            # @constraint(model, t1 == x[j] - x[i])
            # @constraint(model, t2 == x[l] - x[k])
            # @constraint(model, t1 != t2)
        end
    end
    
    # Symmetry breaking
    @constraint(model, x[2] - x[1] <= x[n] - x[n-1])

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
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end # end golomb_ruler2

# @time golomb_ruler2(8,true,false) # ??
for n in 2:10 
    println("\n:$n")
    @time status = golomb_ruler2(n,true,false) # 
    if status == MOI.TIME_LIMIT
        break 
    end
end    