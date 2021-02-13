#=
  Steiner triplets in ConstraintSolver.jl

  http://www.probp.com/examples/clpset/steiner.pl
  """
  The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements 
  in {1,2,...,n} such that each set contains three elements and any two 
  sets have at most one element in common.

  For example, the following shows a solution for size n=7:

      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

  Problem taken from:
  C. Gervet: Interval Propagation to Reason about Sets: Definition and 
             Implementation of a PracticalLanguage,  
             Constraints, An International Journal, vol.1, pp.191-246, 1997.
  """


  Note: This model uses arrays of booleans as an representation of sets.
  See steiner2.jl which use integers and is faster.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function steiner(n=7,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],
                                                            "seed" => 4,

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            # "branch_strategy" => :IMPS, # default
                                                            "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 1, # default, 10
                                                            "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    if !(n % 6 == 1 || n % 6 == 3)
        println("n must be (1|3) modulo 6")
        return
    end

    nb = round(Int,(n * (n-1)) / 6) # number of sets
    println("n:$n nb:$nb")

    @variable(model, x[1:nb,1:n], Bin)
    @constraint(model, x[1,1] == 1) # symmetry breaking

    # atmost 1 element in common
    for i in 1:nb
        @constraint(model,sum(x[i,:]) == 3)
        # @constraint(model, sum([x[i,k] == 1 && x[j,k] == 1 for k in 1:n]) <= 1) # to my wishlist!

        for j in i+1:nb
            # b1 = @variable(model, [1:n], Bin)
            # b2 = @variable(model, [1:n], Bin)
            b = @variable(model, [1:n], Bin)
            for k in 1:n 
                # @constraint(model, b1[k] := {x[i,k] == 1})
                # @constraint(model, b2[k] := {x[j,k] == 1})
                # @constraint(model, b[k] := { b1[k] + b2[k] == 2 })

                @constraint(model, b[k] := { x[i,k] == 1 && x[j,k] == 1 })
            end
            @constraint(model, sum(b) <= 1)
        end
    end
    

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                # Convert to ints
                println([ [j for j in 1:n if x_val[i,j] == 1] for i in 1:nb   ])
            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

# @time steiner(7,true,false)
for n in [i for i in 3:20 if (i % 6 == 1 || i % 6 == 3)]
    @time status, num_sols = steiner(n,true,false,6)
    println()
    if status == MOI.TIME_LIMIT 
        break
    end 
end