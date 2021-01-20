#=

  Magic squares and cards in ConstraintSolver.jl 

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function magic_square_and_cards(n=3,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>all_solutions, 
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

    @variable(model, 1 <= x[1:n,1:n] <= 13, Int)
    @variable(model, 0 <= s <= 13*4, Int) # the sum

    # there are 4 cards of each value in a deck
    for k in 1:13 
        b = @variable(model, [1:n,1:n], Bin)
        for i in 1:n, j in 1:n 
            @constraint(model, b[i,j] := {x[i,j] == k})
        end
        @constraint(model, sum(b) <= 4)
    end
 
    # the standard magic square constraints (sans all_different)
    for i in 1:n 
        @constraint(model, sum(x[:,i]) == s)
        @constraint(model, sum(x[i,:]) == s)
    end

    # diagonals
    @constraint(model,sum([x[i,i] for i in 1:n]) == s)
    @constraint(model,sum([x[i,n+1-i] for i in 1:n]) == s)

    # Symmetry breaking 
    @constraint(model, x[1,1] .<= x[:,:]) 

    @objective(model,Max,s)

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
                s_val = convert.(Integer,JuMP.value.(s; result=sol))
                println("s:$s_val")
                # println("x:$x_val")
                for r in eachrow(x_val)
                    println(r)
                end
                println()


            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

function test(r)
    all_optimal_solutions = true 
    for n in r
        println("\nn:$n")
        if n > 4
            all_optimal_solutions = false
        end
        @time magic_square_and_cards(n, true,all_optimal_solutions,60)
    end
end

test(2:4)
