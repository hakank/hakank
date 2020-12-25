#=

  Sicherman Dice in Julia ConstraintSolver.jl 

  From http://en.wikipedia.org/wiki/Sicherman_dice
  """ 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  """

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:
  
  x1 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
  x2 = array1d(1..6, [1, 2, 3, 4, 5, 6]);
  ----------
  x1 = array1d(1..6, [1, 2, 2, 3, 3, 4]);
  x2 = array1d(1..6, [1, 3, 4, 5, 6, 8]);


  Extra: If we also allow 0 (zero) as a valid value then the 
  following two solutions are also valid:
  
  x1 = array1d(1..6, [0, 1, 1, 2, 2, 3]);
  x2 = array1d(1..6, [2, 4, 5, 6, 7, 9]);
  ----------
  x1 = array1d(1..6, [0, 1, 2, 3, 4, 5]);
  x2 = array1d(1..6, [2, 3, 4, 5, 6, 7]);
  
  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html


  Note: This model currently only show one solution.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function sicherman_dice(n=6,max_val=10,min_val=1,print_solutions=true,all_solutions=true)

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

                                                            # "simplify"=>false, # default true

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            "lp_optimizer" => ipopt_optimizer,
                                        ))
    # standard distribution of 2 pair of dice
    standard_dist = [1,2,3,4,5,6,5,4,3,2,1]
                                     
    # The two dice
    @variable(model, min_val <= x1[1:n] <= max_val, Int)
    @variable(model, min_val <= x2[1:n] <= max_val, Int)

    # ensure standard distributions of the sums
    for k in 1:11
        # Number of occurrence for k+1 when throwing 2 dice
        #=
        b = @variable(model, [1:n,1:n], Bin)
        for i in 1:n, j in 1:n 
            @constraint(model,b[i,j] := {x1[i] + x2[j] == k+1})
        end
        @constraint(model,standard_dist[k] == sum(b))
        =#
        # This works
        count_ctr(model, [x1[i]+x2[j] for i in 1:n, j in 1:n], :(==), k+1, standard_dist[k])

        # The following might work in a future version of ConstraintSolver.jl
        # @constraint(model,standard_dist[k] === sum([(x1[i]+x2[j] == k+1) for i in 1:n, j in 1:n]))
        
    end

    # Symmetry breaking: x1 is less <= to x2
    for i in 1:n 
        @constraint(model,x1[i] <= x2[i])
    end
    
    # Symmetry breaking
    increasing(model,x1)
    increasing(model,x2)
 

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
                x1_val = convert.(Integer,JuMP.value.(x1; result=sol))
                x2_val = convert.(Integer,JuMP.value.(x2; result=sol))
                println("x1:$x1_val")
                println("x2:$x2_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

n_dice=6
max_val=9
min_val=1
# Currently only one solution
println("\nmin_val: 1")
@time sicherman_dice(n_dice,max_val,min_val,true,false)
println("\nmin_val: 0")
@time sicherman_dice(n_dice,max_val,0,true,false)