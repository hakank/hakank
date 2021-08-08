#=

Cryptarithmetic puzzle in Julia

Prolog benchmark problem GNU Prolog (crypta.pl)
"""
Name           : crypta.pl                                              
Title          : crypt-arithmetic                                       
Original Source: P. Van Hentenryck's book                               
Adapted by     : Daniel Diaz - INRIA France                             
Date           : September 1992                                         

Solve the operation:                                                    

B A I J J A J I I A H F C F E B B J E A                              
+ D H F G A B C D I D B I F F A G F E J E                              
-----------------------------------------                              
= G J E G A C D D H F A F J B F I H E E F                              
"""

Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function crypta_problem(print_solutions=true,all_solutions=true)
    
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
    n = 10
    @variable(model, 0 <= x[1:n] <= 9, Int)
    a,b,c,d,e,f,g,h,i,j = x
    @variable(model, sr1, Bin)
    @variable(model, sr2, Bin)
    
    @constraint(model, x in CS.AllDifferent())
    
    @constraint(model, b >= 1)
    @constraint(model, d >= 1)
    @constraint(model, g >= 1)
    
    
    @constraint(model, a+10*e+100*j+1000*b+10000*b+100000*e+1000000*f+
                      e+10*j+100*e+1000*f+10000*g+100000*a+1000000*f
                      == f+10*e+100*e+1000*h+10000*i+100000*f+1000000*b+10000000*sr1)
    
    @constraint(model, c+10*f+100*h+1000*a+10000*i+100000*i+1000000*j+
                        f+10*i+100*b+1000*d+10000*i+100000*d+1000000*c+sr1
                        == j+10*f+100*a+1000*f+10000*h+100000*d+1000000*d+10000000*sr2)
    
    @constraint(model, a+10*j+100*j+1000*i+10000*a+100000*b+
                        b+10*a+100*g+1000*f+10000*h+100000*d+sr2
                        == c+10*a+100*g+1000*e+10000*j+100000*g)
    
    
    
    # Solve the problem
    optimize!(model)
    
    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
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
    
    return status
end

@time crypta_problem()