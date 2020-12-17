#=
   SEND+MORE=MONEY in Julia + ConstraintSolver.jl

   Model created by Hakan Kjellerstrand, hakank@gmail.com
   See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
const CS = ConstraintSolver
include("constraints_utils.jl")




function send_more_money()
    model = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>true,"logging"=>[]))
    @variable(model, 0 <= x[1:8] <= 9, Int)
    s,e,n,d,m,o,r,y = x

    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, s != 0)
    @constraint(model, m != 0)
    @constraint(model,
                              1000*s + 100*e + 10*n + d
                           +  1000*m + 100*o + 10*r + e
                 == 10000*m + 1000*o + 100*n + 10*e + y
        )

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        x = convert.(Integer,JuMP.value.(x))
        println(x)
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
    end
end

send_more_money()
