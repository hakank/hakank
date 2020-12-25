#=

  Futoshiki problem in Julia ConstraintSolver.jl 
 
  http://en.wikipedia.org/wiki/Futoshiki
  """
  The puzzle is played on a square grid, such as 5 x 5. The objective
  is to place the numbers 1 to 5 (or whatever the dimensions are) such 
  that each row, and column contains each of the digits 1 to 5. Some 
  digits may be given at the start. In addition, inequality
  constraints are also initially specifed between some of the squares, 
  such that one must be higher or lower than its neighbour. These 
  constraints must be honoured as the grid is filled out.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function futoshiki(problem,print_solutions=true,all_solutions=true)

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

    p = problem[:problem]    
    less_than = problem[:less_than]
    n,_ = size(p)

    @variable(model, 1 <= x[1:n,1:n] <= n, Int)

    latin_square(model, x)

    # fill the data 
    for i in 1:n, j in 1:n 
        if p[i,j] > 0 
            @constraint(model, x[i,j] == p[i,j])
        end
    end

    # less than hints 
    for (i1,j1,i2,j2) in eachrow(less_than)
        @constraint(model, x[i1,j1] <= x[i2,j2])
    end

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                # println("x:$x_val")
                display(x_val)

            end
        end
    else
        println("status:$status")
    end

    return status
end

futoshiki_problems = Dict( 
  :1 => Dict(
    # Example from Tailor model futoshiki.param/futoshiki.param
    #
    # Solution:
    # 5 1 3 2 4
    # 1 4 2 5 3
    # 2 3 1 4 5
    # 3 5 4 1 2
    # 4 2 5 3 1
    # 
    # Futoshiki instance, by Andras Salamon
    #
    :problem =>
       resize_matrix([[0,0,3,2,0],  # problem grid
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,0,0,0,0]]),
    
    :less_than => 
       resize_matrix([[1,2,1,1], # [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
        [1,4,1,5],
        [2,3,1,3],
        [3,3,2,3],
        [3,4,2,4],
        [2,5,3,5],
        [3,2,4,2],
        [4,4,4,3],
        [5,2,5,1],
        [5,4,5,3],
        [5,5,4,5]])
   ),
    
    
    # Example from http://en.wikipedia.org/wiki/Futoshiki
    # Solution:
    # 5 4 3 2 1
    # 4 3 1 5 2
    # 2 1 4 3 5
    # 3 5 2 1 4
    # 1 2 5 4 3
    #
:2 => Dict(
    :problem =>
       resize_matrix([[0,0,0,0,0],
        [4,0,0,0,2],
        [0,0,4,0,0],
        [0,0,0,0,4],
        [0,0,0,0,0]]),
    :less_than => 
       resize_matrix([[1,2, 1,1],
        [1,4, 1,3],
        [1,5, 1,4],
        [4,4, 4,5],
        [5,1, 5,2],
        [5,2, 5,3]])
),
    
    
    # From http://www.sudoku-puzzles.net/futoshiki06x6.html
   :3 => Dict(
    :problem => 
       resize_matrix([[5,0,2,0,1,0],
        [6,0,1,0,0,0],
        [0,0,3,0,0,0],
        [0,0,4,0,0,0],
        [0,0,0,4,2,5],
        [0,0,0,1,3,0]]),
    :less_than => 
       resize_matrix([[1,6, 2,6],
        [4,1, 3,1]])
    

    )

)

for p in sort(collect(keys(futoshiki_problems)))
    println("\nProblem $p")
    @time futoshiki(futoshiki_problems[p])
end
