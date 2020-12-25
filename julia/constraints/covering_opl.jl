#=

  Set covering problem in Picat.

  This example is from the OPL example covering.mod
  """
  Consider selecting workers to build a house. The construction of a
  house can be divided into a number of tasks, each requiring a number of
  skills (e.g., plumbing or masonry). A worker may or may not perform a
  task, depending on skills. In addition, each worker can be hired for a
  cost that also depends on his qualifications. The problem consists of
  selecting a set of workers to perform all the tasks, while minimizing the
  cost. This is known as a set-covering problem. The key idea in modeling
  a set-covering problem as an integer program is to associate a 0/1
  variable with each worker to represent whether the worker is hired.
  To make sure that all the tasks are performed, it is sufficient to
  choose at least one worker by task. This constraint can be expressed by a
  simple linear inequality.
  """

  Solution from the OPL model:
  """
  Optimal solution found with objective: 14
  crew= {23 25 26}
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function covering_opl(problem,print_solutions=true,all_solutions=true)

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
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    num_workers = problem[:num_workers]
    qualified   = problem[:qualified]
    cost        = problem[:cost]
    num_tasks = problem[:num_tasks]

    # Which workers to hire
    @variable(model, hire[1:num_workers], Bin)
    @variable(model, 0 <= total_cost <= num_workers*maximum(cost), Int)

    scalar_product(model,cost,hire,total_cost)
    for j in 1:num_tasks
        @constraint(model, sum([hire[w] for w in qualified[j]]) >= 1)
    end

    @objective(model,Min, total_cost)

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
                hire_val = convert.(Integer,JuMP.value.(hire; result=sol))
                total_cost_val = convert.(Integer,JuMP.value.(total_cost; result=sol))
                println("total_cost:$total_cost_val")
                # println("hire:$hire_val")
                println("hire:", [i for i in 1:num_workers if hire_val[i] == 1 ])
                println()


            end
        end
    else
        println("status:$status")
    end

    return status
end


covering_opl_problems = Dict(
  :1 => Dict(
        :num_workers => 32,

        # The workers qualified for the works
        :qualified =>
        [
            [  1,  9, 19, 22, 25, 28, 31 ],
            [  2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 ],
            [  3, 10, 19, 24, 26, 30, 32 ],
            [  4, 21, 25, 28, 32 ],
            [  5, 11, 16, 22, 23, 27, 31 ],
            [  6, 20, 24, 26, 30, 32 ],
            [  7, 12, 17, 25, 30, 31 ] ,
            [  8, 17, 20, 22, 23  ],
            [  9, 13, 14, 26, 29, 30, 31 ],
            [ 10, 21, 25, 31, 32 ],
            [ 14, 15, 18, 23, 24, 27, 30, 32 ],
            [ 18, 19, 22, 24, 26, 29, 31 ],
            [ 11, 20, 25, 28, 30, 32 ],
            [ 16, 19, 23, 31 ],
            [  9, 18, 26, 28, 31, 32 ]
        ],

        :num_tasks => 15,

        # cost per worker
        :cost => [1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3,
                3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9 ]
    )
)

@time covering_opl(covering_opl_problems[:1])
