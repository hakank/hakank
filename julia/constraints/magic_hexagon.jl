#=

  Magic hexagon in ConstraintSolver.jl

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function magic_hexagon(print_solutions=true,all_solutions=true,timeout=6)

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

    n = 19
    @variable(model, 1 <= x[1:n] <= n, Int)
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S = x

    @constraint(model, x in CS.AllDifferentSet())

    @constraint(model, A + B + C ==  38)
    @constraint(model, D + E + F + G ==  38)
    @constraint(model, H + I + J + K + L ==  38,)
    @constraint(model, M + N + O + P ==  38,)
    @constraint(model, Q + R + S ==  38,)
    @constraint(model, A + D + H ==  38,)
    @constraint(model, B + E + I + M ==  38,)
    @constraint(model, C + F + J + N + Q ==  38,)
    @constraint(model, G + K + O + R ==  38,)
    @constraint(model, L + P + S ==  38,)
    @constraint(model, C + G + L ==  38,)
    @constraint(model, B + F + K + P ==  38,)
    @constraint(model, A + E + J + O + S ==  38,)
    @constraint(model, D + I + N + R ==  38,)
    @constraint(model, H + M + Q ==  38,)
    
    @constraint(model, A < H)
    @constraint(model, A < C)
    @constraint(model, A < L)
    @constraint(model, A < Q)
    @constraint(model, A < S)
    @constraint(model, C < H)
   

    # @objective(model,Max,z)

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

@time magic_hexagon()
