#=

  Strimko puzzle in Julia.

  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """
 
  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function strimko(problem,print_solutions=true,all_solutions=true)

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

    streams = problem[:streams]
    placed = problem[:placed]
    n, _ = size(streams)

    @variable(model, 1 <= x[1:n,1:n] <= n, Int)

    latin_square(model, x)

    # streams 
    for s in 1:n 
        positions = [x[i,j] for i in 1:n, j in 1:n if s == streams[i,j]]
        @constraint(model,positions in CS.AllDifferentSet())
    end

    # places 
    for p in eachrow(placed)
        @constraint(model,x[p[1], p[2]] == p[3])
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
                display(x_val)

            end
        end
    else
        println("status:$status")
    end

    return status
end


strimko_problems = Dict(
    #
    # Strimko Monthly #02
    # Via http://www.hakank.org/minizinc/strimko2_002.dzn
    #
    :2  => Dict(
        :streams => resize_matrix([[1,1,2,2,2,2,2],
                   [1,1,2,3,3,3,2],
                   [1,4,1,3,3,5,5],
                   [4,4,3,1,3,5,5],
                   [4,6,6,6,7,7,5],
                   [6,4,6,4,5,5,7],
                   [6,6,4,7,7,7,7]]),
        :placed => resize_matrix([[2,1,1],
                   [2,3,7],
                   [2,5,6],
                   [2,7,4],
                   [3,2,7],
                   [3,6,1],
                   [4,1,4],
                   [4,7,5],
                   [5,2,2],
                   [5,6,6]])
    ),
    
    # 
    # Strimko Weekly Set 067
    # Via http://www.hakank.org/minizinc/strimko2_067.dzn
    :67 => Dict(
       :streams =>  resize_matrix([[1,1,1,2,3],
                 [1,2,2,2,3],
                 [1,2,4,5,3],
                 [5,4,5,4,3],
                 [4,5,5,4,3
                 ]]),
        :placed =>  resize_matrix([[1,3,4],
                [1,4,1],
                [3,3,2],
                [3,5,3],
                [5,4,5]])
    ),
    
    #
    # Strimko Weekly Set 068
    # Via http://www.hakank.org/minizinc/strimko2_068.dzn
    :68 => Dict(
    
        :streams => resize_matrix([[1,2,2,4],
               [2,1,4,2],
               [3,4,1,3],
               [4,3,3,1]]),
        :placed =>  resize_matrix([[2,2,3],
               [2,3,2],
               [3,3,1]])
    ),
    
    # Strimko Weekly Set 069
    # Via http://www.hakank.org/minizinc/strimko2_069.dzn
    :69  => Dict(
        :streams => resize_matrix([[1,2,3,3,3,4],
                  [2,1,3,5,4,3],
                  [2,1,3,5,5,4],
                  [2,6,1,6,5,4],
                  [2,6,1,6,4,5],
                  [6,2,6,1,5,4]]),
        :placed =>  resize_matrix([[2,2,4],
                  [2,3,1],
                  [2,4,3],
                  [2,5,2],
                  [3,2,1],
                  [3,5,6],
                  [4,3,5],
                  [4,4,2]])
    ),
    
    # Strimko Weekly Set 070
    # Via http://www.hakank.org/minizinc/strimko2_070.dzn
    :70 => Dict(
        :streams =>  resize_matrix([[1,2,3,3,3],
                   [2,1,1,3,1],
                   [2,2,3,1,4],
                   [5,2,5,4,4],
                   [5,5,5,4,4]]),
        :placed =>  resize_matrix([[1,1,1],
                   [2,5,4],
                   [4,1,2],
                   [5,4,5]])
    )
    
)

for p in sort(collect(keys(strimko_problems)))
    println("\nProblem $p")
    @time strimko(strimko_problems[p])
end