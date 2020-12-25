#=

  Bales of hay problem in Julia ConstraintSolver.jl

  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.

  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function bales_of_hay(print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=> 6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    n = 5
    weights = [80, 82, 83, 84, 85, 86, 87, 88, 90, 91]
    w_len = length(weights)
    max_x = 50

    @variable(model, 0 <= x[1:n] <= max_x, Int)

    increasing(model,x)
    @variable(model,1 <= bx[1:w_len,1:2] <= max_x,Int)
    for w in 1:length(weights)
        i = @variable(model,integer=true, lower_bound=1, upper_bound=n)
        j = @variable(model,integer=true, lower_bound=1, upper_bound=n)
        @constraint(model,i <= j)
        @constraint(model,i != j)
        my_element(model, i, x, bx[w,1])
        my_element(model, j, x, bx[w,2])
        @constraint(model, sum(bx[w,:]) == weights[w])
    end

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                bx_val = convert.(Integer,JuMP.value.(bx; result=sol))
                println("x:$x_val")
                println("bx:$bx_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

@time bales_of_hay(true,false)
