#=
  Furniture moving (scheduling) in Julia ConstraintSolver.jl

  From Marriott & Stuckey: "Programming with constraints", page  112f

  One result:
     Sp Sc Sb  St
    [0, 0, 30, 45]

  Where the values are the start time for each task:
   Starts with piano time 0  : 3 persons  (30 min)
               chair time 0  : 1 person   (10 min)
               bed   time 30 : 3 persons  (15 min)
               table time 45 : 2 persons  (15 min)

   0       10   15    30      45      60

   piano --------------|bed---|
   piano --------------|       table----|
   piano --------------|bed---|
   chair --|            bed---|table----|

  There are many other solutions...


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
function furniture_moving1(print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))



    # Furniture moving
    n = 4
    # [piano, chair, bed, table]
    durations = [30,10,15,15]
    resources = [3,1,3,2] # resource needed per task
    @variable(model, 0 <= start_times[1:n] <= 60, Int)
    @variable(model, minimum(durations) <= end_times[1:n]   <= 60, Int)
    @variable(model, 1 <= limit <= 3, Int)
    @variable(model, 0 <= max_time <= 60+maximum(durations),Int)

    for i in 1:n
        @constraint(model,end_times[i] == start_times[i] + durations[i])
    end
    my_max(model, end_times, max_time)
    cumulative(model, start_times, durations, resources, limit)

    # This is very slow
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
                    println("Task $i: $(start_timesx[i])..$(end_timesx[i]) (duration:$(durations[i]) resource:$(resources[i])")
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

print_solutions=true
all_solutions=false
@time furniture_moving1(print_solutions,all_solutions)
