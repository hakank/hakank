#=

  Bus scheduling in Julia ConstraintSolver.jl

  Problem from Taha "Introduction to Operations Research", page 58.
  Scheduling of buses during a day.

  This is a slightly more general model than Taha's.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function bus_schedule(demands,print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    timeslots = length(demands)

    # result: how many buses start the schedule at time slot t?
    @variable(model, 0 <= x[1:timeslots] <= sum(demands), Int)
    @variable(model, 0 <= num_buses <= sum(demands), Int)

    # meet the demands for this and the next time slot
    for i in 1:timeslots-1
        @constraint(model, x[i]+x[i+1] >= demands[i])
    end
    # demand "around the clock"
    @constraint(model, x[timeslots]+x[1] >= demands[timeslots])

    @constraint(model, num_buses == sum(x))

    # Symmetry breaking
    my_min(model, x, x[1])

    @objective(model,Min, num_buses)

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
                num_buses_val = convert.(Integer,JuMP.value.(num_buses; result=sol))
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$x_val num_buses:$num_buses_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end


# demand: minimum number of buses at time t
demands = [8, 10, 7, 12, 4, 4]
@time bus_schedule(demands,true,true)
