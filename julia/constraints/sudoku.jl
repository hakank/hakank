#=
   Sudoku in Julia + ConstraintSolver

   This is based on the Sudoku example in
   https://wikunia.github.io/ConstraintSolver.jl/stable/tutorial/

   Model created by Hakan Kjellerstrand, hakank@gmail.com
   See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")
include("sudoku_problems.jl")


function sudoku(grid,print_solution=true,timeout=Inf,all_solutions=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    # Solver options:
    # https://wikunia.github.io/ConstraintSolver.jl/stable/options/
    # m = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>true,"logging"=>[]))
    m = Model(optimizer_with_attributes(CS.Optimizer,
                                        "all_solutions"=>all_solutions,
                                        "logging"=>[],

                                        "traverse_strategy"=>:BFS,
                                        # "traverse_strategy"=>:DFS,
                                        # "traverse_strategy"=>:DBFS,

                                        "branch_split"=>:Smallest,
                                        # "branch_split"=>:Biggest,
                                        # "branch_split"=>:InHalf,

                                        # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                        "branch_strategy" => :IMPS, # default
                                        # "branch_strategy" => :ABS, # Activity Based Search
                                        # "activity.decay" => 0.999, # default 0.999
                                        # "activity.max_probes" => 100, # default, 10
                                        # "activity.max_confidence_deviation" => 20, # default 20

                                        # "simplify"=>false,
                                        # "simplify"=>true,
                                        "time_limit"=>timeout,

                                        # "lp_optimizer" => cbc_optimizer,
                                        # "lp_optimizer" => glpk_optimizer,
                                        # "lp_optimizer" => ipopt_optimizer,

                                        ))

    n,_ = size(grid)  # 9
    ns = round(Int, sqrt(n)) # 3
    n2 = ns-1 # 2


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

    # Alterntive using transpose (just checking)
    # xt = x' # Transposed version
    # for rc = 1:n
    #    @constraint(m, x[rc,:] in CS.AllDifferentSet())
    #    @constraint(m, xt[rc,:] in CS.AllDifferentSet())
    # end

    # Cell constraints
    for i in 1:ns:n, j in 1:ns:n
        @constraint(m, vec([x[i+k,j+l] for k in 0:n2, l in 0:n2]) in CS.AllDifferentSet())
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
        # num_sols = MOI.get(m, MOI.ResultCount())
        # println("num_sols:$num_sols")
        return x
    end
end

#
# Solve the problems in list a
# The problems are in sudoku_problems.jl
#
function check_sudoku(problems, a::Array,print_solutions=true, timeout=Inf)
    println("a:$a")
    for p in a
        println("p:$p")
        p2 = resize_matrix(problems[p])
        @time sudoku(p2,print_solutions,timeout)
    end
end

#
# Solve all problems of size n x n
# The problems are in sudoku_problems.jl
#
function check_sudoku(problems, n::Int,print_solutions=true,timeout=Inf)
    num_problems = 0
    total_time = 0
    for p in sort(collect(keys(problems)),by=k->string(k))
        pp = problems[p]
        len = length(pp)
        if len == n
            num_problems += 1
            println("\n\nproblem $p")
            p2 = resize_matrix(problems[p])
            t = @timed @time sudoku(p2,print_solutions,timeout)
            total_time += t.time
        end
    end
    println("\nnum_problems:$num_problems total_time:$total_time")
end

#
# Solve Norvig's 95 Sudoku's from http://norvig.com/top95.txt
#
# 0.47s
function norvig_problems(print_solutions=false)
   file = "top95.txt"
   # Try to download the file if not exist
   if !isfile(file)
       println("$file does not exist. Trying to download it from http://norvig.com/top95.txt")
       tmp = download("http://norvig.com/top95.txt")
       mv(tmp,file)
       println("downloaded $file")
   end
   if !isfile(file)
       println("$file does not exist and download() didn't work. Please download from http://norvig.com/top95.txt")
       return
   end
   lines = readlines(file).|>line->replace(line,"."=>"0")
   c = 0
   for line in lines
       c += 1
       p = reshape(split(line,"").|>i->parse(Int,i),(9,9))
       @time sudoku(p,print_solutions)
   end
   println("Solve $c Sudoku instances.")

end

timeout = 600.0 # seconds. Only needed for the 25x25 instances
println("Timeout: $timeout")
print_solutions=false

#
# Run the 9x9 instances
#
@time check_sudoku(all_sudoku_problems, 9, print_solutions,timeout) # 40 problems, 0.33s

#
# Run the 16x16 instances
#
@time check_sudoku(all_sudoku_problems, 16,print_solutions,timeout) # 55 problems: 0.59s

#
# Run the 25x25 instances
# Note The 25x25 takes a lots of time so we have to use a timeout when running them.
#
#=
   With a timeout of 600s (10 min)
   - #18: Timeout
   - #19: 146.5s
   - #20: 515.9s
   - #21-#34: Timeout
   - #89: 3.8s
   - #90: 7.3s
=#
# @time check_sudoku(all_sudoku_problems, 25, true,timeout) # test all instances
@time check_sudoku(all_sudoku_problems, [19,89,90],true, timeout) # just test the simplest problems


#
# Run Norvig's 95 Sudoku problems: 0.47s
#
# @time norvig_problems(false)
#
