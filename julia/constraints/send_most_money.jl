#=
  SEND+MOST=MONEY in Julia + ConstraintSolver.jl

  Alphametic problem were we maximize MONEY.

  This version do two things:
    - find the maximum of MONEY
    - and then find all solutions for the maximum value of MONEY.

  Problem from the lecture notes:
  http://www.ict.kth.se/courses/ID2204/notes/L01.pdf

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
const CS = ConstraintSolver
include("constraints_utils.jl")

function send_most_money()
    model = Model(optimizer_with_attributes(CS.Optimizer,
                                                        # "all_solutions"=>true,
                                                        "all_optimal_solutions"=>true,
                                                        "logging"=>[]))
    @variable(model, 0 <= x[1:8] <= 9, Int)
    s,e,n,d,m,o,t,y = x

    @variable(model, 10000 <= MONEY <= 99999, Int)

    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, s != 0)
    @constraint(model, m != 0)
    @constraint(model, MONEY == 10000*m + 1000*o + 100*n + 10*e + y)
    @constraint(model,
                              1000*s + 100*e + 10*n + d
                           +  1000*m + 100*o + 10*s + t
                 == MONEY
        )

    @objective(model,Max,MONEY)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        for sol in 1:num_sols
            println("solution #$sol")
            xx = convert.(Integer,JuMP.value.(x; result=sol))
            MONEYx = convert.(Integer,JuMP.value.(MONEY; result=sol))
            println("x:$xx MONEY:$MONEYx\n")
        end

    end
end

send_most_money()
