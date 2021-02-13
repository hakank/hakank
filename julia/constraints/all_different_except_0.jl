#=

  Decomposition of global constraint alldifferent_except_0 in Julia + ConstraintSolver.

  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  """
  Enforce all variables of the collection VARIABLES to take distinct
  values, except those variables that are assigned to 0.

  Example
     (<5, 0, 1, 9, 0, 3>)

  The alldifferent_except_0 constraint holds since all the values
  (that are different from 0) 5, 1, 9 and 3 are distinct.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc
const CS = ConstraintSolver
include("constraints_utils.jl")

# Note: The all_different_except_c is defined in constraints_utils.jl

function all_different_except_0_test(n=5)
    println("n:$n")
    model = Model(optimizer_with_attributes(CS.Optimizer,
                                            "all_solutions"=>true,
                                            # "all_solutions"=>false,
                                            "logging"=>[],
                                            "time_limit"=>10,
                                            )
                                            )
    @variable(model, 0 <=  x[1:n] <= n, Int)

    all_different_except_c(model,x,0)
    increasing(model, x)

    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL

        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")

        for sol in 1:num_sols
            println("solution #$sol")
            xx = convert.(Integer,JuMP.value.(x,result=sol))
            println("x:$xx")

        end

    end

end

all_different_except_0_test()
