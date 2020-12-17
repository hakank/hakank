#=
  Quasigroup completion in Julia + ConstraintSolver

  See
  Carla P. Gomes and David Shmoys:
 "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"


  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these
  numbers into a four-by-four array so that no column or row contains
  the same two numbers. The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is
  correctly but only partially filled in. The question is whether the
  remaining blanks in the table can be filled in to obtain a complete
  Latin square (or a proper quasigroup multiplication table).
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc
const CS = ConstraintSolver
include("constraints_utils.jl")
include("quasigroup_completion_problems.jl")

function quasigroup_completion(grid,print_solution=true,all_solutions=false,timeout=Inf)
    n,_ = size(grid)  # 9

    m = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>all_solutions,"logging"=>[],
                                                      "traverse_strategy"=>:BFS, # Seems to be quite fast!
                                                      # "traverse_strategy"=>:DFS,
                                                      # "traverse_strategy"=>:DBFS,

                                                      "branch_split"=>:Smallest, # Seems to be the best
                                                      # "branch_split"=>:Biggest,
                                                      # "branch_split"=>:InHalf,

                                                      # "simplify"=>false,
                                                      # "simplify"=>true,
                                                      "time_limit"=>timeout
                                                      ))

    # define the variables
    @variable(m, 1 <= x[1:n,1:n] <= n, Int)

    # hints
    for r=1:n, c=1:n
        if grid[r,c] != 0
            @constraint(m, x[r,c] == grid[r,c])
        end
    end

    # Row and column constraints
    for rc = 1:n
        @constraint(m, x[rc,:] in CS.AllDifferentSet())
        @constraint(m, x[:,rc] in CS.AllDifferentSet())
    end

    # Solve the problem
    optimize!(m)

    status = JuMP.termination_status(m)
    println("status:$status")
    if status == MOI.OPTIMAL
        x = convert.(Integer,JuMP.value.(x))
        if print_solution
            print_grid(x)
        end
        num_sols = MOI.get(m, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        return x
    end
end

print_solutions=true
all_solutions=false
timeout=10
for p in sort(collect(keys(quasigroup_completion_problems)))
    println("\nproblem $p")
    if p in [5,6,7]
        # These instances has too many solutions
        all_sols = false
    else
        all_sols = true
    end
    @time quasigroup_completion(resize_matrix(quasigroup_completion_problems[p]),print_solutions,all_sols,timeout)

    println("\n")
end
