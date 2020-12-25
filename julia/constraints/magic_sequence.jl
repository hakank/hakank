#=

  Magic sequence problem in Julia ConstraintSolver.jl 

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

# Algorithmic approach
function magic_sequence_alg(n::Int64) 
    s = zeros(Int64,n)
    s[1:3] .= n-4,2,1
    s[end-3] = 1
    return s
end

function magic_sequence(n=4,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

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
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    @variable(model, 0 <= x[1:n] <= n-1, Int)

    @constraint(model, sum(x) == n)
    scalar_product(model,collect(0:n-1),x,n)

    for i in 0:n-1 
        count_ctr(model,x,:(==),i,x[i+1])
    end

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

for n in 2:30 
    print("n: $n  ")
    @time magic_sequence(n)
end

println("\nAlgorithmic approach")
@time magic_sequence_alg(100_000_000)
