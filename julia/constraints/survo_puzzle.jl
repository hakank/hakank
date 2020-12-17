#=

  Survo puzzle in Julia + ConstraintSolver.jl

  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's
  Survo system which is a general environment for statistical computing and
  related areas.

  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so
  that each of these numbers appears only once and their row and column sums are
  equal to integers given on the bottom and the right side of the table.
  Often some of the integers are given readily in the table in order to
  guarantee uniqueness of the solution and/or for making the task easier.
  """

  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html

  References:
  Mustonen, S. (2006b). "On certain cross sum puzzles"
  http://www.survo.fi/papers/puzzles.pdf
  Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles."
  http://www.survo.fi/papers/enum_survo_puzzles.pdf
  Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles"
  http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc
const CS = ConstraintSolver
include("constraints_utils.jl")
include("survo_puzzle_problems.jl")

function survo_puzzle(p,print_solution=true,all_solutions=false,timeout=Inf)
    row_sums = p[:row_sums]
    col_sums = p[:col_sums]
    println("row_sums: $row_sums col_sums:$col_sums")
    grid = resize_matrix(p[:problem])
    rows,cols = size(grid)  # 9

    m = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>all_solutions,"logging"=>[],
                                        # "traverse_strategy"=>:BFS, # Seems to be quite fast!
                                        # "traverse_strategy"=>:DFS,
                                        # "traverse_strategy"=>:DBFS,

                                        # "branch_split"=>:Smallest, # Seems to be the best
                                        # "branch_split"=>:Biggest,
                                        # "branch_split"=>:InHalf,

                                        # "simplify"=>false,
                                        # "simplify"=>true,
                                        "time_limit"=>timeout
                                        ))

    # define the variables
    @variable(m, 1 <= x[1:rows,1:cols] <= rows*cols, Int)

    # hints
    for r=1:rows, c=1:cols
        if grid[r,c] != 0
            @constraint(m, x[r,c] == grid[r,c])
        end
    end

    @constraint(m, vec(x[:,:]) in CS.AllDifferentSet())

    # Row and column constraints
    for r = 1:rows
        @constraint(m, row_sums[r] == sum(x[r,:]))
    end
    for c = 1:cols
        @constraint(m, col_sums[c] == sum(x[:,c]))
    end

    # Solve the problem
    optimize!(m)

    status = JuMP.termination_status(m)
    println("status:$status")
    if status == MOI.OPTIMAL
        x = convert.(Integer,JuMP.value.(x))
        if print_solution
            # display(@show x)
            print_grid(x)
        end
        num_sols = MOI.get(m, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        return x
    end
end



print_solution=true
all_solutions=true
timeout=10

for p in sort(collect(keys(survo_puzzle_problems)))
    println("\nproblem $p")
    @time survo_puzzle(survo_puzzle_problems[p],print_solutions,all_solutions,timeout)

    println("\n")
end
