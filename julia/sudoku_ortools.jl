#=
   Port of my the Or-tools/Python Sudoku solver. 

   After warmup:
   julia> @time include("sudoku_or_tools.jl")
    num_solutions: 1
    [8, 6, 1, 4, 5, 9, 7, 2, 3]
    [4, 5, 2, 3, 1, 7, 6, 9, 8]
    [7, 9, 3, 6, 8, 2, 5, 1, 4]
    [2, 1, 6, 8, 3, 5, 4, 7, 9]
    [9, 8, 4, 2, 7, 6, 1, 3, 5]
    [3, 7, 5, 1, 9, 4, 8, 6, 2]
    [5, 4, 7, 9, 2, 1, 3, 8, 6]
    [1, 3, 9, 5, 6, 8, 2, 4, 7]
    [6, 2, 8, 7, 4, 3, 9, 5, 1]

    num_solutions:1
    failures:0
    branches:0
    WallTime:291
    0.297074 seconds (571.88 k allocations: 29.771 MiB, 3.09% gc time)

   Via command line: 
   $ time julia sudoku_or_tools.jl
    num_solutions: 1
    [8, 6, 1, 4, 5, 9, 7, 2, 3]
    [4, 5, 2, 3, 1, 7, 6, 9, 8]
    [7, 9, 3, 6, 8, 2, 5, 1, 4]
    [2, 1, 6, 8, 3, 5, 4, 7, 9]
    [9, 8, 4, 2, 7, 6, 1, 3, 5]
    [3, 7, 5, 1, 9, 4, 8, 6, 2]
    [5, 4, 7, 9, 2, 1, 3, 8, 6]
    [1, 3, 9, 5, 6, 8, 2, 4, 7]
    [6, 2, 8, 7, 4, 3, 9, 5, 1]

    num_solutions:1
    failures:0
    branches:0
    WallTime:839
    julia sudoku_or_tools.jl  2,63s user 0,78s system 155% cpu 2,193 total

=#
using PyCall

# Resize matrix to "normal" form
function resize_matrix(grid)
    rows = size(grid)
    transpose(reduce(hcat,grid)) # Suggestion by @Mason Protter @ Zulip
end


# Be sure that ortools are installed correctly
# Note that we are using the old CP solver (not the new hot SAT solver)
ot = pyimport("ortools.constraint_solver.pywrapcp")

# Create the solver
solver = ot.Solver("Sudoku")

function sudoku(initial_grid)
    cell_size = 3
    line_size = cell_size^2
    line = 1:line_size
    cell = 1:cell_size

    # Decision variables
    grid = [solver.IntVar(1, line_size, "grid[$i,$j]") for i in line, j in line]

    # AllDifferent on rows.
    for i in line
        solver.Add(solver.AllDifferent([grid[i, j] for j in line]))
    end

    # AllDifferent on columns.
    for j in line
        solver.Add(solver.AllDifferent([grid[i, j] for i in line]))
    end 

    # AllDifferent on cells.
    for i in cell, j in cell
        solver.Add(solver.AllDifferent(
                        vec([grid[(i-1) * cell_size + di, (j-1) * cell_size + dj] for di in cell, dj in cell] )
                    ))
    end
    
    # Initial values.
    for i in line
        for j in line
            if initial_grid[i,j] > 0
                solver.Add(grid[i, j] == initial_grid[i,j])
            end
        end 
    end

    # Regroup all variables into a list.
    all_vars = [grid[i, j] for i in line for j in line]

    # Create search phases.
    vars_phase = solver.Phase(all_vars,
                                solver.INT_VAR_SIMPLE,
                                solver.INT_VALUE_SIMPLE)

    solution = solver.Assignment()
    solution.Add(all_vars)
    # Note: This give just one solution, even if there are many others
    collector = solver.FirstSolutionCollector(solution)

    # And solve.
    solver.Solve(vars_phase, [collector])
    
    num_solutions = collector.SolutionCount()
    println("num_solutions: $num_solutions")
    if num_solutions == 1
        for i in line
            println([collector.Value(0, grid[i, j]) for j in line])
        end 
    end
    println("\nnum_solutions:", num_solutions)
    println("failures:", solver.Failures())
    println("branches:", solver.Branches())
    println("WallTime:", solver.WallTime())

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

sudoku(initial_grid)
