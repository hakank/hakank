#=

  Curious numbers in ConstraintSolver.jl 

  """
  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

  The number 48 has this peculiarity, that if you add 1 to it the result
  is a square number, and if you add 1 to its half, you also get a
  square number. Now, there is no limit to the numbers that have this
  peculiarity, and it is an interesting puzzle to find three more of
  them---the smallest possible numbers. What are they?
  """ 


  The least such numbers are: 
  [
   [48,49,7,24,25,5],
   [1680,1681,41,840,841,29],
   [57120,57121,239,28560,28561,169], 
   [1940448,1940449,1393,970224,970225,985]
  ]

  Note: This model use the experimental (and very slow)
  non-linear mult constraint (mult(model,A,B,C) -> C = A*B)
  It thus only handle the first two results shows above.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function cur_num(max_val=100,print_solutions=true,all_solutions=true,timeout=6)

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

    n = 6
    @variable(model, 1 <= x[1:n] <= max_val, Int)
    X,A,B,C,D,E = x

    @constraint(model,X + 1 == A) # if you add 1 to it 
    # @constraint(model,A == B * B) # the result is a square number
    mult(model,B,B,A)
    
    # @cons)raint(model,X == 2 * C) # if you to its half
    mult(model,2,C,X)
    @constraint(model,C + 1 == D) # add 1 
    # @constraint(model,D == E * E) # you also get a square number
    mult(model,E,E,D)
  
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
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

# for max_val in [100,2000]
for max_val in [100,2000]    
    println("\nmax_val:$max_val")
    @time cur_num(max_val)
end

