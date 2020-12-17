#=

  Set covering problem in Julia ConstraintSolver.jl

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering3(problem,print_solutions=true,all_solutions=true)

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
                                                            # "activity.max_probes" => 100, # default, 10
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
    belongs = problem[:belongs]
    num_groups,num_senators = size(belongs)

    # Which senator to select
    @variable(model, x[1:num_senators], Bin)
    @variable(model, 0 <= min_val <= num_senators, Int)

    # cover all groups with senators
    for i in 1:num_groups
        @constraint(model, sum(x.*belongs[i,:])>=1)
    end

    @constraint(model, min_val == sum(x))
    
    @objective(model,Min,min_val)
    
    # Solve the problem
    optimize!(model)
    
    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                min_val_val = convert.(Integer,JuMP.value.(min_val; result=sol))
                println("min_val:$min_val_val")
                println("x:$x_val")
                println("senators selected:$([i for i in 1:num_senators if x_val[i] == 1])")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

problem = Dict(
    :belongs => resize_matrix([
        [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   # 1 southern
        [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   # 2 northern
        [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   # 3 liberals
        [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   # 4 conservative
        [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   # 5 democrats
        [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]    # 6 republicans
    ]                                   )
)

@time set_covering3(problem,true,true)
