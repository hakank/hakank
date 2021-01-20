#=

  "A puzzle" in ConstraintSolver.jl 

  From "God plays dice"
  "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/

  And the sequel "Answer to a puzzle"
  http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/

  This problem instance was taken from the latter blog post.

  """
  8809 = 6
  7111 = 0
  2172 = 0
  6666 = 4
  1111 = 0
  3213 = 0
  7662 = 2
  9312 = 1
  0000 = 4
  2222 = 0
  3333 = 0
  5555 = 0
  8193 = 3
  8096 = 5
  7777 = 0
  9999 = 4
  7756 = 1
  6855 = 3
  9881 = 5
  5531 = 0

  2581 = ?
  """

  Note: 
  This model yields 10 solutions, since x[5] is not 
  restricted in the constraints. 
  All solutions has x assigned to the correct result. 
  

  The problem stated in "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
  is
  """
  8809 = 6
  7662 = 2
  9312 = 1
  8193 = 3
  8096 = 5
  7756 = 1
  6855 = 3
  9881 = 5

  2581 = ?
  """

  This problem instance - using the same principle - yields 
  two different solutions of x, one is the same (correct) as 
  for the above problem instance, and one is not.
  This is because here both x[5] and x[2] are underdefined.
  
  Note: 
  This problem has another non-algebraic and - let's say - topological
  approach which yield the same solution as the first problem and one
  of the two solutions of the second problem.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function a_puzzle(problem,print_solutions=true,all_solutions=true,timeout=6)

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

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    m = problem[:m]
    p = problem[:p] # the unknown

    n = 10
    @variable(model, 0 <= x[1:n] <= 9, Int)
    @variable(model, 0 <= z <= 9, Int)


    for i in 1:length(m)
        num, a = m[i] 
        ds = split(num,"").|>d->parse(Int,d)
        @constraint(model, a == sum([x[ds[j]+1] for j in 1:4 ]))
    end
    # Find the unknown
    @constraint(model, z == sum([x[p[j]+1] for j in 1:4 ]))

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("x:$x_val  z:$z_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

problems = Dict(
    :1 => Dict( 
        :m =>  [["8809",6],
                ["7111", 0],
                ["2172", 0],
                ["6666", 4],
                ["1111", 0],
                ["3213", 0],
                ["7662", 2],
                ["9312", 1],
                ["0000", 4],
                ["2222", 0],
                ["3333", 0],
                ["5555", 0],
                ["8193", 3],
                ["8096", 5],
                ["7777", 0],
                ["9999", 4],
                ["7756", 1],
                ["6855", 3],
                ["9881", 5],
                ["5531", 0]],
        :p => [2,5,8,1], # The unknown

    ),

    :2 => Dict(
        :m => [
            ["8809",6],
            ["7662",2],
            ["9312",1],
            ["8193",3],
            ["8096",5],
            ["7756",1],
            ["6855",3],
            ["9881",5],
        ],
        :p => [2,5,8,1]

    )

)

println("Problem I:")
@time a_puzzle(problems[:1],true,true)


println("\nProblem II:")
@time a_puzzle(problems[:2],true,true)

