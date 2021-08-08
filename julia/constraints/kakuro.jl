#=

  Kakuru puzzle in Julia ConstraintSolver.jl

  http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive
  into each white cell such that the sum of the numbers in each entry
  matches the clue associated with it and that no digit is duplicated in
  any entry. It is that lack of duplication that makes creating Kakuro
  puzzles with unique solutions possible, and which means solving a Kakuro
  puzzle involves investigating combinations more, compared to Sudoku in
  which the focus is on permutations. There is an unwritten rule for
  making Kakuro puzzles that each clue must have at least two numbers
  that add up to it. This is because including one number is mathematically
  trivial when solving Kakuro puzzles; one can simply disregard the
  number entirely and subtract it from the clue it indicates.
  """

  This model solves the problem at the Wikipedia page.
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

  The solution:
    9 7 0 0 8 7 9
    8 9 0 8 9 5 7
    6 8 5 9 7 0 0
    0 6 1 0 2 6 0
    0 0 4 6 1 3 2
    8 9 3 1 0 1 4
    3 1 2 0 0 2 1


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function kakuro(problem,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            # "activity.decay" => 0.999, # default 0.999
                                                            # "activity.max_probes" => 100, # default, 10
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
    n = problem[:size]
    println("n:$n")
    hints = problem[:hints]
    blanks = problem[:blanks]

    @variable(model, 0 <= x[1:n,1:n] <= 9, Int)

    # Blanks
    for r in 1:n, c in 1:n
        if [r,c] in blanks
            @constraint(model,x[r,c] == 0)
        else
            # If not in blanks then it cannot be 0
            @constraint(model,x[r,c] != 0)
        end
    end

    # Hints
    for hint in hints
        s, lst = hint
        xs = [x[i,j] for (i,j) in lst]
        @constraint(model, s == sum(xs))
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
# Hints
#
kakuro_problems = Dict(
:1 => Dict(
  :size => 7,
  # [Sum, [List of indices in X]]
  :hints =>  [
      [16, [[1,1],[1,2]]],
      [24, [[1,5],[1,6],[1,7]]],
      [17, [[2,1],[2,2]]],
      [29, [[2,4],[2,5],[2,6],[2,7]]],
      [35, [[3,1],[3,2],[3,3],[3,4],[3,5]]],
      [ 7, [[4,2],[4,3]]],
      [ 8, [[4,5],[4,6]]],
      [16, [[5,3],[5,4],[5,5],[5,6],[5,7]]],
      [21, [[6,1],[6,2],[6,3],[6,4]]],
      [ 5, [[6,6],[6,7]]],
      [ 6, [[7,1],[7,2],[7,3]]],
      [ 3, [[7,6],[7,7]]],

      [23, [[1,1],[2,1],[3,1]]],
      [30, [[1,2],[2,2],[3,2],[4,2]]],
      [27, [[1,5],[2,5],[3,5],[4,5],[5,5]]],
      [12, [[1,6],[2,6]]],
      [16, [[1,7],[2,7]]],
      [17, [[2,4],[3,4]]],
      [15, [[3,3],[4,3],[5,3],[6,3],[7,3]]],
      [12, [[4,6],[5,6],[6,6],[7,6]]],
      [ 7, [[5,4],[6,4]]],
      [ 7, [[5,7],[6,7],[7,7]]],
      [11, [[6,1],[7,1]]],
      [10, [[6,2],[7,2]]]
  ],

   # indices of blanks
   :blanks =>
      [
       [1,3], [1,4],
       [2,3],
       [3,6], [3,7],
       [4,1], [4,4],[4,7],
       [5,1], [5,2],
       [6,5],
       [7,4], [7,5]
   ],
  ),

)

@time kakuro(kakuro_problems[:1],true,true)
