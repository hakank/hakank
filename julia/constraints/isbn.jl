#=

  Some explorations of ISBN13 in ConstraintSolver.jl 

  See http://en.wikipedia.org/wiki/ISBN
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function isbn(isbn,print_solutions=true,all_solutions=true,timeout=6)

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

    n = 13
    M = -1 # unknown
    mult1 = 3 
    mult2 = 1
    @variable(model, 0 <= x[1:n] <= 9, Int)
    @variable(model, 0 <= t[1:n-1] <= 9*3, Int) # For the checksum

    # ISBN starts with 978 or 979
    @constraint(model, x[1] == 9)
    @constraint(model, x[2] == 7)
    @constraint(model, x[3] >= 8) 
    
    
    for i in 1:n
        if isbn[i] != M 
            @constraint(model, x[i] == isbn[i])
        end
        # Checksum
        if i < n 
            if i % 2 == 0
                @constraint(model, t[i] == x[i]*mult1)
            else 
                @constraint(model, t[i] == x[i]*mult2)
            end
        end
    end
    
    #
    # Checksum: x[n] = (10 - tsum mod 10) mod 10
    #
    @variable(model, 0 <= tsum <= 3*9*13, Int)
    @variable(model, 0 <= tsum_mod_10 <= 9, Int)
    tsum_minus_10 = @variable(model, integer=true,lower_bound=0, upper_bound=9)
    
    @constraint(model,tsum == sum(t))
    @variable(model, 10 <= ten <= 10, Int)
    # ten = 10
    modulo(model,tsum,ten,tsum_mod_10)
    @constraint(model,tsum_minus_10 == 10 - tsum_mod_10)
    modulo(model,tsum_minus_10,ten,x[n])

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
                # t_val = convert.(Integer,JuMP.value.(t; result=sol))
                # println("t:$t_val")
                # println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end


M = -1 # unknown

# Test ISBN:
# 978-0262720304: The OPL Optimization Programming Language 
# [9,7,8,0,2,6,2,7,2,0,3,0,4]
test1 = [9,7,8,0,2,6,2,7,2,0,3,0,M]
println("len:", length(test1))
#
# isbn = 978-0262220774: Constraint-based Local Search
# [9,7,8,0,2,6,2,2,2,0,7,7,4]
test2 = [9,7,8,0,2,6,2,2,2,0,7,7,M]

# Constraint Solving and Planning with Picat
# book: http://www.springer.com/gp/book/9783319258812
# [9,7,8,3,3,1,9,2,5,8,8,1,2]
test3 = [9,7,8,3,3,1,9,2,5,8,8,1,M]
test4 = [9,7,8,3,3,1,9,2,5,8,8,M,M]

@time isbn(test1,true,true)
@time isbn(test2,true,true)
@time isbn(test3,true,true)
@time isbn(test4,true,true)
