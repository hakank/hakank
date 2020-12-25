#=

  Mr Smith problem in Julia ConstraintSolver.jl 

  From an IF Prolog example (http://www.ifcomputer.de/)
  """
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
      o If Mr Smith comes, his wife will come too.
      o At least one of their two sons Matt and John will come.
      o Either Mrs Smith or Tim will come, but not both.
      o Either Tim and John will come, or neither will come.
      o If Matt comes, then John and his father will
        also come.
  """

  The answer should be:
    Mr_Smith_comes      =  0
    Mrs_Smith_comes     =  0
    Matt_comes          =  0
    John_comes          =  1
    Tim_comes           =  1

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function mr_smith(print_solutions=true,all_solutions=true)

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

    n = 5
    @variable(model, x[1:n], Bin)
    mr_smith,mrs_smith,matt,john,tim = x
    people = ["mr_smith","mrs_smith","matt","john","tim"]

    # If Mr Smith comes, his wife will come too.
    @constraint(model,mr_smith => {mrs_smith == 1})
 
    # At least one of their two sons Matt and John will come.
    @constraint(model,matt + john >= 1)
 
    # Either Mrs Smith or Tim will come, but not both.
    # Mrs_Smith + Tim #= 1,
    @constraint(model,mrs_smith + tim == 1)
 
    # Either Tim and John will come, or neither will come.
    @constraint(model,tim == john)
 
    # If Matt comes, then John and his father will also come.
    @variable(model, john_and_mr_smith, Bin)
    @constraint(model, john_and_mr_smith := {john + mr_smith == 2})
    @constraint(model,matt => {john_and_mr_smith == 1})
 

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
                println("x:$x_val")
                println("These will come:", [people[i] for i in 1:n if x_val[i] == 1])
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time mr_smith()