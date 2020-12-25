#=

  Fill-a-pix problem in Julia ConstraintSolver.jl 

  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """
 
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function fill_a_pix(problem,print_solutions=true,all_solutions=true)

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
    n,_ = size(problem)

    @variable(model, x[1:n,1:n], Bin)

    for i in 1:n, j in 1:n 
        if problem[i,j] >= 0 
            @constraint(model, problem[i,j] == 
                            sum([x[i+a,j+b] for a in -1:1, b in -1:1 
                            if 1 <= i+a <= n &&
                               1 <= j+b <= n
                            ])
            )
        end
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
                print_matrix_str(x_val,".X")

            end
        end
    else
        println("status:$status")
    end

    return status
end

M = -1
fill_a_pix_problems = Dict(
    # Puzzle 1 from 
    # http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
    # 
    :1 => resize_matrix([[M,M,M,M,M,M,M,M,0,M],
               [M,8,8,M,2,M,0,M,M,M],
               [5,M,8,M,M,M,M,M,M,M],
               [M,M,M,M,M,2,M,M,M,2],
               [1,M,M,M,4,5,6,M,M,M],
               [M,0,M,M,M,7,9,M,M,6],
               [M,M,M,6,M,M,9,M,M,6],
               [M,M,6,6,8,7,8,7,M,5],
               [M,4,M,6,6,6,M,6,M,4],
               [M,M,M,M,M,M,3,M,M,M]]
          ),
    
    # Puzzle 2 from 
    # http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
    # 
    :2 => resize_matrix([[0,M,M,M,M,M,3,4,M,3],
                [M,M,M,4,M,M,M,7,M,M],
                [M,M,5,M,2,2,M,4,M,3],
                [4,M,6,6,M,2,M,M,M,M],
                [M,M,M,M,3,3,M,M,3,M],
                [M,M,8,M,M,4,M,M,M,M],
                [M,9,M,7,M,M,M,M,5,M],
                [M,M,M,7,5,M,M,3,3,0],
                [M,M,M,M,M,M,M,M,M,M],
                [4,4,M,M,2,3,3,4,3,M]]),
    
    
    # Puzzle from 
    # http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
    #
    # Code: 030.15x15
    # ID: 03090000000
    # 
    :3 => resize_matrix([[M,5,M,6,M,M,M,M,M,M,6,M,M,M,M],
                [M,M,7,6,M,4,M,M,4,M,M,8,9,M,5],
                [5,M,M,5,M,5,M,3,M,6,M,7,M,M,6],
                [4,M,2,M,4,M,4,M,3,M,2,M,M,9,M],
                [M,M,M,5,M,4,M,3,M,4,M,4,5,M,6],
                [M,4,3,3,4,M,M,M,4,M,2,M,M,M,M],
                [M,M,M,M,M,M,M,M,M,5,M,M,M,4,M],
                [3,M,3,M,M,3,M,M,M,5,M,4,4,M,M],
                [M,M,M,4,3,M,3,3,M,M,5,7,6,M,M],
                [4,M,M,M,2,M,3,3,2,M,8,9,M,5,M],
                [M,M,3,M,M,M,M,5,M,M,7,M,8,M,M],
                [4,M,M,3,2,M,M,M,M,M,7,M,M,6,M],
                [M,M,4,M,5,4,4,M,M,9,6,M,M,M,M],
                [M,3,5,7,M,6,M,M,M,M,M,M,7,M,M],
                [M,M,4,6,6,M,M,M,6,5,M,M,M,4,M]]),
    
    
    
    
)

for p in sort(collect(keys(fill_a_pix_problems)))
    println("\nproblem $p")
    @time fill_a_pix(fill_a_pix_problems[p])
end
