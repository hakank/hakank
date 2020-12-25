#=

  Discrete tomography in Julia ConstraintSolver.jl

  Note: The origin of the problem is from ECLiPSe,
  but this model has been transformed in this way
     MiniZinc -> SICStus Prolog -> ECLiPSe -> B-Prolog -> Picat -> ConstraintSolver.jl
  Here is my own take at the problem.

  Problem from http://eclipse-clp.org/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.

  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:

  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0
  0
  8      * * * * * * * *
  2      *             *
  6      *   * * * *   *
  4      *   *     *   *
  5      *   *   * *   *
  3      *   *         *
  7      *   * * * * * *
  0
  0


  Eclipse solution by Joachim Schimpf, IC-Parc
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# print_matrix_str(x,zstring)
#
# Print each row and convert each element to the corresponding position
# in zstring. "z" is for zero-based, i.e. first char corresponds to 0, etc
#
function print_matrix_str(x,zstring="")
    for row in eachrow(x)
        if zstring == ""
            println(row)
        else
            println(join(row.|>c->zstring[c+1]))
        end
    end
    println()
end

function discrete_tomography(problem,print_solutions=true,all_solutions=true)

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
                                                            # "branch_strategy" => :IMPS, # default
                                                            "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 100, # default, 10
                                                            "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    row_sums = problem[:r]
    col_sums = problem[:c]

    rows = length(row_sums)
    cols = length(col_sums)

    @variable(model, x[1:rows,1:cols], Bin)

    # Check rows and cols
    for i in 1:rows
        @constraint(model,row_sums[i] == sum(x[i,:]))
    end
    for j in 1:cols
        @constraint(model,col_sums[j] == sum(x[:,j]))
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
                print_matrix_str(x_val,".X")


            end
        end
    else
        println("status:$status")
    end

    return status
end

discrete_tomography_problems = Dict(

#
# The three first problems are from the ECLiPSe model:
#
# The above stated problem
:1  => Dict(
       :r => [0,0,8,2,6,4,5,3,7,0,0], # row sums
       :c => [0,0,7,1,6,3,4,5,2,7,0,0] # column sums
       ),

:2 => Dict(
       :r => [10,4,8,5,6],
       :c => [5,3,4,0,5,0,5,2,2,0,1,5,1]
),

# This give three slightly different solutions.
:3 => Dict(
        :r => [11,5,4],
        :c => [3,2,3,1,1,1,1,2,3,2,1]
),

# This is my own problem.
:4 => Dict(
        :r => [0,2,2,2,2,2,8,8,4,4,4,4,4,0],
        :c => [0,0,0,12,12,2,2,2,2,7,7,0,0,0]
),

)

for p in sort(collect(keys(discrete_tomography_problems)))
    println("problem $p")
    @time discrete_tomography(discrete_tomography_problems[p])
end
