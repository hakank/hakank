#=

  Killer Sudoku in Julia ConstraintSolver.jl

  http://en.wikipedia.org/wiki/Killer_Sudoku
  """
  Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or
  samunamupure) is a puzzle that combines elements of sudoku and kakuro.
  Despite the name, the simpler killer sudokus can be easier to solve
  than regular sudokus, depending on the solver's skill at mental arithmetic;
  the hardest ones, however, can take hours to crack.

  ...

  The objective is to fill the grid with numbers from 1 to 9 in a way that
  the following conditions are met:

    * Each row, column, and nonet contains each number exactly once.
    * The sum of all numbers in a cage must match the small number printed
      in its corner.
    * No number appears more than once in a cage. (This is the standard rule
      for killer sudokus, and implies that no cage can include more
      than 9 cells.)

  In 'Killer X', an additional rule is that each of the long diagonals
  contains each number once.
  """

  Here we solve the problem from the Wikipedia page, also shown here
  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  The output is:
    2 1 5 6 4 7 3 9 8
    3 6 8 9 5 2 1 7 4
    7 9 4 3 8 1 6 5 2
    5 8 6 2 7 4 9 3 1
    1 4 2 5 9 3 8 6 7
    9 7 3 8 1 6 4 2 5
    8 2 1 7 3 9 5 4 6
    6 5 9 4 2 8 7 1 3
    4 3 7 1 6 5 2 8 9

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


#
# Basic Sudoku model
#
function sudoku(model,x,n)
    # Row and column constraints
    for rc = 1:n
        @constraint(model, x[rc,:] in CS.AllDifferent())
        @constraint(model, x[:,rc] in CS.AllDifferent())
    end

    # Cell constraints
    ns = round(Int, sqrt(n))
    n2 = ns-1
    for i in 1:ns:n, j in 1:ns:n
        @constraint(model, vec([x[i+k,j+l] for k in 0:n2, l in 0:n2]) in CS.AllDifferent())
    end

end

function killer_sudoku(problem,print_solutions=true,all_solutions=true)

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

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 100, # default, 10
                                                            "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    n = 9
    # define the variables
    @variable(model, 1 <= x[1:n,1:n] <= n, Int)

    sudoku(model, x, n) # plain Sudoku model

    # The Killer Sudoku hints
    for hint in problem
        s,lst = hint
        xs = [x[t[1],t[2]] for t in lst]
        @constraint(model,s == sum(xs))
        @constraint(model, xs in CS.AllDifferent())
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
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                display(x_val)

            end
        end
    else
        println("status:$status")
    end

    return status
end

#
# hints
#
killer_sudoku_problems = Dict(
   :1 => [
            #  [Sum, [list of indices in X]]
            [ 3, [[1,1], [1,2]]],
            [15, [[1,3], [1,4], [1,5]]],
            [22, [[1,6], [2,5], [2,6], [3,5]]],
            [ 4, [[1,7], [2,7]]],
            [16, [[1,8], [2,8]]],
            [15, [[1,9], [2,9], [3,9], [4,9]]],
            [25, [[2,1], [2,2], [3,1], [3,2]]],
            [17, [[2,3], [2,4]]],
            [ 9, [[3,3], [3,4], [4,4]]],
            [ 8, [[3,6], [4,6],[5,6]]],
            [20, [[3,7], [3,8],[4,7]]],
            [ 6, [[4,1], [5,1]]],
            [14, [[4,2], [4,3]]],
            [17, [[4,5], [5,5],[6,5]]],
            [17, [[4,8], [5,7],[5,8]]],
            [13, [[5,2], [5,3],[6,2]]],
            [20, [[5,4], [6,4],[7,4]]],
            [12, [[5,9], [6,9]]],
            [27, [[6,1], [7,1],[8,1],[9,1]]],
            [ 6, [[6,3], [7,2],[7,3]]],
            [20, [[6,6], [7,6], [7,7]]],
            [ 6, [[6,7], [6,8]]],
            [10, [[7,5], [8,4],[8,5],[9,4]]],
            [14, [[7,8], [7,9],[8,8],[8,9]]],
            [ 8, [[8,2], [9,2]]],
            [16, [[8,3], [9,3]]],
            [15, [[8,6], [8,7]]],
            [13, [[9,5], [9,6],[9,7]]],
            [17, [[9,8], [9,9]]]
            ]

)

@time killer_sudoku(killer_sudoku_problems[:1])
