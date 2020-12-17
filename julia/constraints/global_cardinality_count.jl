#=
  Decomposition of global constraint global_cardinality in Julia ConstraintSolver.

  See Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html
  """
  Example:
  <3,3,8,6>,
   (val-3 noccurrence-2,
    val−5 noccurrence-0,
    val-6 noccurrence-1)

  The global_cardinality constraint holds since values 3, 5 and 6
  respectively occur 2, 0 and 1 times within the collection
  <3,3,8,6> and since no constraint was specified for value 8.
  """

  Notes:
     - The version here is limited. See below for details.
     - Picat has already a global_cardinality/2, but the details
       of the parameters are different.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")


#
# Note: global_cardinality_count is defined in constraints_utils.jl
#
function gcc_test(n=5,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))

    @variable(model, 1 <= a[1:n] <= n, Int)
    @variable(model, 0 <= gcc[1:n] <= n, Int)

    global_cardinality_count(model, a, gcc)


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
                ax = convert.(Integer,JuMP.value.(a; result=sol))
                gccx = convert.(Integer,JuMP.value.(gcc; result=sol))
                # println("position:$positionx\n")
                println("a:$ax gcc:$gccx\n")
            end
        end
    else
        println("status:$status")
    end
    return status
end

@time gcc_test(5)
