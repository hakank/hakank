#=

  Organize a day in Julia ConstraintSolver.jl 

  Problem formulation:
  Slides on (finite domain) Constraint Logic Programming, page 38f

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Printf
const CS = ConstraintSolver
include("constraints_utils.jl")

   


function organize_day(problem,print_solutions=true,all_solutions=true)

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
    tasks = problem[:tasks]
    durations = problem[:durations]
    precedences = problem[:precedences]
    start_time = problem[:start_time]
    end_time = problem[:end_time]


    n = length(tasks)
    @variable(model, start_time <= begins[1:n] <= end_time, Int)
    @variable(model, start_time <= ends[1:n] <= end_time, Int)

    for i in 1:n 
        @constraint(model,ends[i] == begins[i] + durations[i])
    end
    
    no_overlap(model,begins,durations)

    # precedences 
    println("precedences:$precedences")
    for (a,b) in eachrow(precedences)
        @constraint(model, ends[a] <= begins[b])
    end

    @constraint(model,begins[1] >= 11)


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
                begins_val = convert.(Integer,JuMP.value.(begins; result=sol))
                ends_val = convert.(Integer,JuMP.value.(ends; result=sol))
                # println("begins:$begins_val ")
                # println("ends  :$ends_val ")
                for t in 1:n 
                    # println("$(tasks[t]): $(begins_val[t]) .. $(ends_val[t])")
                    @printf "%s: %2d .. %2d\n" tasks[t] begins_val[t] ends_val[t]
                end
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

# Problem instance
organize_day_problem = Dict(

    #     task id   1      2      3      4
    :tasks => ["Work","Mail","Shop","Bank"],

    # duration of the four tasks
    :durations => [4,1,2,1],

    # precedences
    # [A,B] : task A must be completed before task B
    :precedences => resize_matrix([[4,3],
                                   [2,1]]
                                  ),
    # Time limits
    :start_time => 9,
    :end_time => 17
)


@time organize_day(organize_day_problem)