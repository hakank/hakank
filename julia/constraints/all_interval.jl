#=
  All interval problem in Julia ConstraintSolver.jl

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly
  once and in which the musical intervals between neighbouring notes cover
  the full set of intervals from the minor second (1 semitone) to the major
  seventh (11 semitones). That is, for each of the intervals, there is a
  pair of neigbhouring pitch-classes in the series, between which this
  interval appears. The problem of finding such a series can be easily
  formulated as an instance of a more general arithmetic problem on Z_n,
  the set of integer residues modulo n. Given n in N, find a vector
  s = (s_1, ..., s_n), such that (i) s is a permutation of
  Z_n = {0,1,...,n-1}; and (ii) the interval vector
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is
  called an all-interval series of size n; the problem of finding such
  a series is the all-interval series problem of size n. We may also be
  interested in finding all possible series of a given size.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# Note: my_abs is defined in constraints_utils.jl
#
function all_interval(n=5,all_solutions=true,print_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer, # 45.8s
                                                            "lp_optimizer" => glpk_optimizer, # 21.5s
                                        ))
    n1 = n-1
    @variable(model, 1 <= x[1:n] <= n, Int)
    @variable(model, 1 <= diffs[1:n1] <= n1, Int)

    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, diffs in CS.AllDifferentSet())

    # diffsb = @variable(model, [1:n1], Bin)
    for k in 1:n1
        # These don't work!
        # @constraint(model, diffs[k] == abs(x[k+1] - x[k])) # abs() is not supported!
        # @constraint(model, diffs[k] == (x[k+1] >= x[k]) ? x[k+1] - x[k] : x[k] - x[k+1]) # This don't work!

        # This works!
        # @constraint(model, diffsb[k] := {x[k+1] >= x[k]})
        # @constraint(model, diffsb[k] := {diffs[k] == x[k+1] - x[k]})
        # @constraint(model, !diffsb[k] := {diffs[k] == x[k] - x[k+1]})

        # This works as well and it seems to be about as fast/slow...
        my_abs(model,x[k],x[k+1],diffs[k])
    end

    # symmetry breaking
    @constraint(model, x[1] <= x[n1])
    @constraint(model, diffs[1] <= diffs[2])

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
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                diffsx = convert.(Integer,JuMP.value.(diffs; result=sol))
                println("x:$xx diffs:$diffsx\n")
            end
        end
    else
        println("status:$status")
    end
end

all_solutions = true
print_solutions = true
@time all_interval(9,all_solutions,print_solutions)

# println("n:13")
# @time all_interval(13,false,print_solutions)

# Count the number of solutions
#=
n:3
num_sols:1

  0.001692 seconds (6.08 k allocations: 363.805 KiB)

n:4
num_sols:1

  0.002169 seconds (11.64 k allocations: 652.992 KiB)

n:5
num_sols:3

  0.003961 seconds (26.33 k allocations: 1.381 MiB)

n:6
num_sols:8

  0.013505 seconds (95.76 k allocations: 4.858 MiB)

n:7
num_sols:9

  0.047034 seconds (364.65 k allocations: 17.449 MiB)

n:8
num_sols:15

  0.206969 seconds (1.73 M allocations: 76.011 MiB)

n:9
num_sols:42

  1.171571 seconds (8.01 M allocations: 346.716 MiB, 6.85% gc time)

n:10
num_sols:104

 14.749069 seconds (54.33 M allocations: 2.278 GiB, 8.40% gc time)
=#
for n in 3:10
    println("\nn:$n")
    @time all_interval(n,true,false)
end
