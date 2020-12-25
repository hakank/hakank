#=

  Marathon puzzle in Julia ConstraintSolver.jl

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  """
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.
  
     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function marathon2(print_solutions=true,all_solutions=true)

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

    
    n = 6
    names = ["Dominique", "Ignace", "Naren", "Olivier", "Philippe", "Pascal"]
    
    @variable(model, 1 <= runners[1:n] <= n, Int)
    dominique, ignace, naren, olivier, philippe, pascal = runners
 
    @constraint(model, runners in CS.AllDifferentSet())
   
    # a: olivier not last
    @constraint(model, olivier != n)
 
    # b: dominique, pascal and ignace before naren and olivier
    @constraint(model, dominique  <= naren)
    @constraint(model, dominique  <= olivier)
    @constraint(model, pascal     <= naren)
    @constraint(model, pascal     <= olivier)
    @constraint(model, ignace     <= naren)
    @constraint(model, ignace     <= olivier)
    
    # c: dominique better than third
    @constraint(model, dominique  <= 3)
    
    # d: philippe is among the first four
    @constraint(model, philippe   <= 4)
    
    # e: ignace neither second nor third
    @constraint(model, ignace     != 2) 
    @constraint(model, ignace     != 3) 
    
    # f: pascal three places earlier than naren
    @constraint(model, pascal + 3 == naren)
    
    # g: neither ignace nor dominique on fourth position
    @constraint(model, ignace     != 4)
    @constraint(model, dominique  != 4)
 
    # for the presentation (inverse)
    @variable(model,1 <= runners_inv[1:n] <= n, Int)
    assignment_ctr(model,runners, runners_inv)
 
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
                runners_val = convert.(Integer,JuMP.value.(runners; result=sol))
                runners_inv_val = convert.(Integer,JuMP.value.(runners_inv; result=sol))
                println("runners:$runners_val")
                println("runners_inv:$runners_inv_val")
                for i in 1:n 
                    println("Place $i: $(names[runners_inv_val[i]])")
                end
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time marathon2(true,true)
