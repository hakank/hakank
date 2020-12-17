#=
  Test of cumulative constraint in Julia ConstraintSolver.jl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP, Printf
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# The cumulative constraint is in constraints_utils.jl
#
function cumulative_test(problem, print_solutions=true,all_solutions=false,time_limit=600)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>time_limit,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    n             = problem[:n]
    problem_name  = problem[:name]
    durations     = problem[:durations]
    resources     = problem[:resources]
    start_time_lb = problem[:start_time_domain][1]
    start_time_ub = problem[:start_time_domain][2]
    end_time_lb   = 0
    end_time_ub   = start_time_ub+maximum(durations)
    limit_lb  = problem[:limit_domain][1]
    limit_ub  = problem[:limit_domain][2]

    println("$problem_name (n:$n)")
    println("durations:$durations")
    println("resources:$resources")
    println("start_time_lb:$start_time_lb")
    println("start_time_ub:$start_time_ub")
    println("end_time_lb:$end_time_lb")
    println("end_time_ub:$end_time_ub")
    println("limit_lb:$limit_lb")
    println("limit_ub:$limit_ub")


    @variable(model, start_time_lb <= start_times[1:n] <= start_time_ub, Int)
    @variable(model, end_time_lb   <= end_times[1:n]   <= end_time_ub,   Int)
    @variable(model, limit_lb      <= limit            <= limit_ub,      Int)
    @variable(model, start_time_lb <= max_time         <= end_time_ub,   Int)

    for i in 1:n
        @constraint(model,end_times[i] == start_times[i] + durations[i])
    end
    my_max(model, end_times, max_time)
    cumulative(model, start_times, durations, resources, limit)

    # This don't work: MethodError: no method matching iterate(::Nothing)
    # @objective(model, Min, max_time)
    # @objective(model,Min,limit)

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
                start_timesx = convert.(Integer,JuMP.value.(start_times; result=sol))
                end_timesx = convert.(Integer,JuMP.value.(end_times; result=sol))
                max_timex = convert.(Integer,JuMP.value.(max_time; result=sol))
                limitx = convert.(Integer,JuMP.value.(limit; result=sol))
                println("start_times:$start_timesx")
                println("durations  :$durations")
                println("end_times  :$end_timesx")
                println("resources  :$resources")
                # println("max_time   :$max_timex")
                println("limit      :$limitx")

                for i in 1:n
                    println("Task $i: $(start_timesx[i])..$(end_timesx[i]) (duration:$(durations[i]) resource:$(resources[i]))")
                end

                time_line = minimum(start_timesx):maximum(end_timesx)
                println("\nTime line: (tasks active (resource))")
                for t in time_line
                    @printf "time %3d: " t
                    total_resources = 0
                    for i in 1:length(start_timesx)
                        if start_timesx[i] <= t && t < start_timesx[i]+durations[i]
                            total_resources += resources[i]
                            @printf " %3d (%2d) " i resources[i]
                        else
                            @printf " %3s  %3s " "" ""
                        end
                    end
                    println("  total_resources: $total_resources")
                end
            end
        end
    else
        println("status:$status")
    end

    return status
end

furniture_moving = Dict(
                        :name               => "Furniture moving",
                        :tasks              => ["piano", "chair", "bed", "table"],
                        :n                  => 4,
                        :durations          => [30,10,15,15],
                        :resources          => [3,1,3,2],
                        :start_time_domain  => [0,60],
                        :limit_domain       => [1,3]
)

# From my Picat model cumulative_decomp.pi
test1 = Dict(
             :name => "cumulative_decomp.pi",
             :tasks => [1,2,3,4,5],
             :n     => 5,
             :durations => [3,9,10,6,2],
             :resources => [1,2,1,1,3],
             :start_time_domain => [1,10],
             :limit_domain => [1,3] # [1,8]
)

# From cumulative_test2.pi
# Originally from (simplified)
# http://stackoverflow.com/questions/25170868/search-heuristics-for-cumulatives
# Note: This is currently (ConstraintSolver.jl 0.5.1) too hard for a 10 min time limit
test2 = Dict(
            :name => "cumulative_test2.pi",
            :n     => 20,
            :tasks => collect(1:20),
            :durations => [318, 246, 797, 238, 251, 279, 987, 847, 426, 787,6, 681,465,46, 3, 427,956,657,113,251],
            :resources => [1 for _ in 1:20],
            :start_time_domain => [1,1000],
            :limit_domain => [1,1]

)

# instances = [furniture_moving,test1,test2]
instances = [furniture_moving,test1]
for instance in instances
    println("Problem $(instance[:name])")
    @time cumulative_test(instance)
    println("\n")
end
