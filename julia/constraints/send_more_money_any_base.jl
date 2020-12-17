#=
   SEND+MORE=MONEY any base in Julia + ConstraintSolver.jl

   Number of solutions for base 2:30
   [0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210, 231]



   Model created by Hakan Kjellerstrand, hakank@gmail.com
   See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function send_more_money_any_base(base=10,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,
                                            "all_solutions"=> all_solutions,
                                            # "all_optimal_solutions"=>all_solutions,
                                            "logging"=>[],
                                            
                                            "traverse_strategy"=>:BFS,
                                            # "traverse_strategy"=>:DFS,
                                            # "traverse_strategy"=>:DBFS,
                                            
                                            # "branch_split"=>:Smallest,
                                            # "branch_split"=>:Biggest,
                                            "branch_split"=>:InHalf,
                                            
                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                            # "branch_strategy" => :IMPS, # default
                                            "branch_strategy" => :ABS, # Activity Based Search
                                            # "activity.decay" => 0.999, # default 0.999
                                            # "activity.max_probes" => 100, # default, 10
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

    println("base: $base")
    @variable(model, 0 <= x[1:8] <= base-1, Int)
    s,e,n,d,m,o,r,y = x

    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, s != 0)
    @constraint(model, m != 0)
    
    @constraint(model,
                                 (base^3)*s + (base^2)*e + base*n + d
                               + (base^3)*m + (base^2)*o + base*r + e
                 == (base^4)*m + (base^3)*o + (base^2)*n + base*e + y
        )

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
                println()
            end
        end
        println("base:$base num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status,num_sols
end

num_sols_a = []
for base in 2:30
    local status
    @time status,num_sols = send_more_money_any_base(base,true,true)
    if status == MOI.TIME_LIMIT
        break
    else
        push!(num_sols_a,num_sols)
    end
        
end


println("Num solutions: $num_sols_a")
