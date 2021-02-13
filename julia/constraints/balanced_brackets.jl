#=

  Balanced brackets in ConstraintSolver.jl 
 
  This model all balanced brackets of size m*2.

  See https://en.wikipedia.org/wiki/Catalan_number

  The number of generated solutions for m:
   m        #
   ----------
    1       1
    2       2
    3       5
    4      14
    5      42
    6     132
    7     429
    8    1430
    9    4862
   10   16796
   11   58786
   12  208012
   13  742900
  
  Which - of course - is the Catalan numbers.

  http://oeis.org/search?q=1%2C2%2C5%2C14%2C42%2C132%2C429%2C1430%2C4862%2C16796%2C58786%2C208012&language=english&go=Search
  http://oeis.org/A000108


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function balanced_brackets(m=4,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
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
                                                            # "activity.max_probes" => 10, # default, 10
                                                            # "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            "lp_optimizer" => ipopt_optimizer,
                                        ))
    n = m*2
    t = [1,-1] # +1 for "[", -1 for "]"
    @variable(model, 1 <= x[1:n] <= 2, Int) # 1="[",  2="]"

    # Counter (cumulative sum)
    @variable(model, 0 <= c[1:n] <= n, Int)

    # Leading "["
    @constraint(model, x[1] == 1)
    @constraint(model, c[1] == 1)

    for i in 2:n 
        tt = @variable(model, [1:1], CS.Integers([-1,1]))
        my_element(model,x[i], t, tt[1])
        @constraint(model, c[i] == c[i-1] + tt[1])
    end

    # Concluding "]"
    @constraint(model, x[n] == 2)
    @constraint(model, c[n] == 0)

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
                # println("solution #$sol")
                if print_solutions
                    x_val = convert.(Integer,JuMP.value.(x; result=sol))
                    c_val = convert.(Integer,JuMP.value.(c; result=sol))
                    println("x:$x_val")
                    println("c:$c_val")
                    println()
                end

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

begin 
    @time local status, num_sols = balanced_brackets(4,true,true)
    println("num_sols: $num_sols")

    for m in 1:20
        println("m:$m")
        @time local status, num_sols = balanced_brackets(m,false,true)
        if status == MOI.TIME_LIMIT
            break
        end
    end
end
