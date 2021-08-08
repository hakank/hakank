#=

  Broken weights problem in Julia ConstraintSolver.jl 

  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie’s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Printf 
const CS = ConstraintSolver
include("constraints_utils.jl")

function broken_weights(n=4,m=40,print_solutions=true,all_solutions=true)

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

    @variable(model, 1 <= weights[1:n] <= m, Int)
    @variable(model, -1 <= x[1:m,1:n] <= 1, Int)

    @constraint(model, weights in CS.AllDifferent())
    @constraint(model, m == sum(weights))

    increasing(model,weights)
    @constraint(model, m == sum(weights))

    
    table = resize_matrix([ [i,j,i * j] for i in -m:m, j in -m:m])    
    for j in 1:m 
        # scalar_product(model,x[j,:],weights,j) # This is a non-linear constraint!
        # This reformulation works.
        t = @variable(model, [1:n], CS.Integers(-m:m))
        for i in 1:n 
            @constraint(model, [x[j,i], weights[i], t[i]] in CS.TableSet(table))
        end
        @constraint(model, j == sum(t))
    end

    @objective(model,Min,weights[n])

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
                weights_val = convert.(Integer,JuMP.value.(weights; result=sol))
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("weights:$weights_val")
                # display(x_val)
                for j in 1:m
                    @printf "%2d: " j
                    for i in 1:n 
                        @printf "%2d" x_val[j,i]
                    end
                    println()
                end
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time broken_weights(4,40)
