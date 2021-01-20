#
# This is a port (with some simplifications) of Google OR-tools Sudoku (SAT) solver 
# https://github.com/google/or-tools/blob/stable/examples/python/sudoku_sat.py
#
# Compared to sudoku_or_tools.jl, here we use the newer SAT solver which has a 
# little different API from the older CP solver pywrapcp.
#
# For debugging: Most of the methods are available via Julia's help 
# (which is really neat). E.g. 
#  
# help?> model.AddAllDifferent
# """
# Adds AllDifferent(variables).
#
# This constraint forces all variables to have different values.
#
# Args:
#   variables: a list of integer variables.
#
# Returns:
#   An instance of the `Constraint` class.
# """
#

# A run:
#= 
julia> @time solve_sudoku(initial_grid)

status:OPTIMAL
[8, 6, 1, 4, 5, 9, 7, 2, 3]
[4, 5, 2, 3, 1, 7, 6, 9, 8]
[7, 9, 3, 6, 8, 2, 5, 1, 4]
[2, 1, 6, 8, 3, 5, 4, 7, 9]
[9, 8, 4, 2, 7, 6, 1, 3, 5]
[3, 7, 5, 1, 9, 4, 8, 6, 2]
[5, 4, 7, 9, 2, 1, 3, 8, 6]
[1, 3, 9, 5, 6, 8, 2, 4, 7]
[6, 2, 8, 7, 4, 3, 9, 5, 1]

Resonse stats:
CpSolverResponse:
status: OPTIMAL
objective: 0
best_bound: 0
booleans: 0
conflicts: 0
branches: 0
propagations: 0
integer_propagations: 0
restarts: 0
lp_iterations: 0
walltime: 0.00166678
usertime: 0.00166675
deterministic_time: 0
primal_integral: 0

  0.012847 seconds (3.79 k allocations: 149.188 KiB)
=#

#
using PyCall

# Resize matrix to "normal" form
function resize_matrix(grid)
    rows = size(grid)
    transpose(reduce(hcat,grid)) # Suggestion by @Mason Protter @ Zulip
end

# Be sure that ortools are installed correctly
cp = pyimport("ortools.sat.python.cp_model")

"Solves the sudoku problem with the CP-SAT solver."
function solve_sudoku(initial_grid)
    
    # Create the model.
    model = cp.CpModel()

    cell_size = 3
    line_size = cell_size^2
    line = 1:line_size
    cell = 1:cell_size

    grid = [model.NewIntVar(1, line_size, "grid[$i,$j]") for i in line, j in line]

    # AllDifferent on rows.
    for i in line
        model.AddAllDifferent([grid[i, j] for j in line])
    end

    # AllDifferent on columns.
    for j in line
        model.AddAllDifferent([grid[i, j] for i in line])
    end

    # AllDifferent on cells.
    for i in cell, j in cell
        model.AddAllDifferent(
                        vec([grid[(i-1) * cell_size + di, (j-1) * cell_size + dj] for di in cell, dj in cell] )
                    )
    end
 
    # Initial values.
    for i in line, j in line
        if initial_grid[i,j] > 0
            model.Add(grid[i, j] == initial_grid[i,j])
        end 
    end

    # Solve and print out the solution.
    solver = cp.CpSolver()
    status = solver.Solve(model)
    println("status:", solver.StatusName())
    if status == cp.OPTIMAL
        for i in line
            println([solver.Value(grid[i, j]) for j in line])
        end 
    end
    println()
    println("Resonse stats:\n", solver.ResponseStats())

    # Return the model and solver since it's instructive to see what they contain...
    return model,solver 

end 

initial_grid = resize_matrix([[0, 6, 0, 0, 5, 0, 0, 2, 0], 
                              [0, 0, 0, 3, 0, 0, 0, 9, 0],
                              [7, 0, 0, 6, 0, 0, 0, 1, 0], 
                              [0, 0, 6, 0, 3, 0, 4, 0, 0],
                              [0, 0, 4, 0, 7, 0, 1, 0, 0], 
                              [0, 0, 5, 0, 9, 0, 8, 0, 0],
                              [0, 4, 0, 0, 0, 1, 0, 0, 6], 
                              [0, 3, 0, 0, 0, 8, 0, 0, 0],
                              [0, 2, 0, 0, 4, 0, 0, 5, 0]])


@time model,solver = solve_sudoku(initial_grid)


