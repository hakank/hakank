#=
   DONALD+GERALD=ROBERT in ConstraintSolver.jl

   Model created by Hakan Kjellerstrand, hakank@gmail.com
   See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
const CS = ConstraintSolver
include("constraints_utils.jl")

function donald_gerald_robert()
    model = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>true,"logging"=>[]))
    @variable(model, 0 <= x[1:10] <= 9, Int)
    d, o, n, a, l, g, e, r, b, t = x

    @constraint(model, x in CS.AllDifferent())
    @constraint(model, d != 0)
    @constraint(model, g != 0)
    @constraint(model, r != 0)
    @constraint(model,
                              100000*d + 10000*o + 1000*n + 100*a + 10*l + d +
                              100000*g + 10000*e + 1000*r + 100*a + 10*l + d ==
                              100000*r + 10000*o + 1000*b + 100*e + 10*r + t

                              # Different approach:
                              # sum([100000*d,10000*o,1000*n,100*a,10*l + d]) +
                              # sum([100000*g,10000*e,1000*r, 100*a, 10*l, d]) ==
                              # sum([100000*r,10000*o, 1000*b, 100*e, 10*r, t])

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

donald_gerald_robert()
