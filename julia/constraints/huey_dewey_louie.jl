#=

  Huey, Dewey and Louie problem in ConstraintSolver.jl
 
  From Marriott & Stuckey, Programming with Constraints, page 42
  """
  Huey, Dewey and Louie are being questioned by their uncle. These are the 
  statements the make:
   Huey: Dewey and Louie has equal share in it; if one is quitly, so
         is the other.
   Dewey: If Huey is guilty, then so am I.
   Louie: Dewey and I are not both quilty.
  
  Their uncle, knowing that they are cub scouts, realises that they
  cannot tell a lie. Has he got sufficient information to decide who 
  (if any) are quilty?
  """
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function huey_dewey_louie(print_solutions=true,all_solutions=true,timeout=6)

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

    n = 3 
    @variable(model, x[1:n], Bin)
    Huey,Dewey,Louie = x

    #  Huey: Dewey and Louie has equal share in it; if one is 
    # quitly, so is the other.
    # Dewey == 1 <=> Louie == 1
    @variable(model, b1, Bin)
    @constraint(model, b1 := {Dewey ==1})
    @constraint(model, b1 := {Louie ==1})
   
    # Dewey: If Huey is guilty, then so am I.
    # Huey ==1 => Dewey ==1
    @variable(model, b2, Bin)
    @constraint(model, b2 := {Huey ==1})
    @constraint(model, b2 => {Dewey ==1})
 
    #  Louie: Dewey and I are not both quilty.
    # ~(Dewey ==1  /\ Louie ==1)
    @variable(model, b3, Bin)
    @variable(model, b4, Bin)
    @constraint(model, b3 := {Dewey == 1})
    @constraint(model, b4 := {Louie == 1})
    @constraint(model, b3+b4 < 2)
 
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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time huey_dewey_louie()
