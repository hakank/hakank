#=

  Subset sum problem in Julia ConstraintSolver.jl

  From Katta G. Murty: "Optimization Models for Decision Making", page 340
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  """
  Example 7.8.1
  
  A bank van had several bags of coins, each containing either
  16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
  street, thieves stole some bags. A total of 100 coins were lost.
  It is required to find how many bags were stolen.
  """

  And we also solve some larger random problem instances.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Distributions
const CS = ConstraintSolver
include("constraints_utils.jl")

function subset_sum_model(a,total,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,
                                            "all_solutions"=> all_solutions,
                                            # "all_optimal_solutions"=>all_solutions,
                                            "logging"=>[],
                                            
                                            # "traverse_strategy"=>:BFS,
                                            # "traverse_strategy"=>:DFS,
                                            "traverse_strategy"=>:DBFS,
                                            
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

    if print_solutions
        println("a: $a")
        println("total: $total")
    end
    
    n = length(a)
    @variable(model, 0 <= x[1:n] <= n, Int)
    # @variable(model, 0 <= s <= n*maximum(a), Int)    

    # scalar_product(model,x,a,total)
    
    @constraint(model,total == sum(a.*x))

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                # s_val = convert.(Integer,JuMP.value.(s; result=sol))                
                println("x:$x_val")
                # println("s:$s_val")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

coins = [16, 17, 23, 24, 39, 40]
total = 100
@time subset_sum_model(coins,total,true,true)

#
# Some random instances
#
println("\nrandom instances")
for _ in 1:10
    # some random examples
    n = rand(DiscreteUniform(10,1_000))
    a = rand(DiscreteUniform(1,n),n)
    tot = rand(DiscreteUniform(1,3*sum(a)))
    println("\nn:$n total:$tot")
    # println("a:$a")
    @time subset_sum_model(a,tot,false,false)
end
