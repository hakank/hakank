#=
  to_num in Julia + ConstraintSolver.jl

  to_num(List, Base, Num) converts a list of integers to a number for
  a base Base. It is bidirectional but it is really recommended that
  the length of List is fixed.

  See examples below.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# to_num and increasing are defined in constraints_utils.jl
#
function to_num_test(n=5,base=10)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> true,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer, # 45.8s
                                                            # "lp_optimizer" => glpk_optimizer, # 21.5s
                                        ))

    @variable(model, 0 <= x[1:n] <= base-1, Int)
    @variable(model, 0 <= v <= 10^n-1, Int)

    to_num(model, x, base, v)
    increasing(model, x)
    @constraint(model, x in CS.AllDifferentSet())

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        xx = convert.(Integer,JuMP.value.(x))
        vv = convert.(Integer,JuMP.value.(v))

        println("x:$xx")
        println("v:$vv")

        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")

        for sol in 1:num_sols
            println("solution #$sol")
            xx = convert.(Integer,JuMP.value.(x,result=sol))
            vv = convert.(Integer,JuMP.value.(v,result=sol))
            println("$xx  <=> $vv\n")

        end
    end
end

to_num_test()
