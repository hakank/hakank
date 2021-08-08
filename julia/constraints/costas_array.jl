#=
Costas array in ConstraintSolver.jl

From http://mathworld.wolfram.com/CostasArray.html:
"""
An order-n Costas array is a permutation on {1,...,n} such
that the distances in each row of the triangular difference
table are distinct. For example, the permutation {1,3,4,2,5}
has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
and {4}. Since each row contains no duplications, the permutation
is therefore a Costas array.
"""
Also see
http://en.wikipedia.org/wiki/Costas_array

This model is based on Barry O'Sullivan's MiniZinc model
(http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn)
Here are the two rather simple differences 
(marked by "hakank" below)
 1) no symmetry breaking on the order of the Costas array
 2) fixes the lower triangular matrix in the difference
    matrix to -n+1

Since there is no symmetry breaking of the order of the Costas 
array it gives all the solutions for a specific length of 
the array, e.g. those 
listed in http://mathworld.wolfram.com/CostasArray.html

1	1	(1)
2	2	(1, 2), (2,1)
3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
              (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
              (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
....

See http://www.research.att.com/~njas/sequences/A008404
for the number of solutions for n=1..
1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
872, 200, 88, 56, 204,...


Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Printf
const CS = ConstraintSolver
include("constraints_utils.jl")

function costas_array(n=8, print_solutions=true,all_solutions=true,timeout=6)

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

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    @variable(model, 1 <= costas[1:n] <= n, Int)
    @variable(model, -n+1 <= differences[1:n,1:n] <= n-1, Int)

    #
    # hakank: Here are my two changes
    #
    # 1) I skipped this constraint since I want 
    #    to generate all solutions.
    # @constraint(model, costas[1] <= costas[N] - 1)
    
    # 2) Fix the values in the lower triangle in the
    # difference matrix to -n+1. This removes variants 
    # of the difference matrix for the the same Costas array.
    for i in 1:n, j in 1:i 
        @constraint(model, differences[i,j] == -n+1)
    end
 
    # hakank: All the following constraints are from 
    # Barry O'Sullivans's original MiniZinc model.
    @constraint(model, costas in CS.AllDifferent())
 
    # "How do the positions in the Costas array relate 
    #  to the elements of the distance triangle."
    for i in 1:n, j in i+1:n
       @constraint(model, differences[i,j] == costas[j] - costas[j-i])
    end
 
    # "All entries in a particular row of the difference 
    #  triangle must be distint."
    for i in 1:n-1
       @constraint(model,[differences[i,j] for j in i+1:n] in CS.AllDifferent())
    end
 
    # "All the following are redundant - only here to speed up search."
    
    # "We can never place a 'token' in the same row as any other."
    for i in 1:n, j in i+1:n 
       @constraint(model, differences[i,j] != 0)
    end
 
    for k in 3:n, l in k+1:n
       @constraint(model, differences[k-2,l-1] + differences[k,l] ==
                          differences[k-1,l-1] + differences[k-1,l])
    end
    
    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                costas_val = convert.(Integer,JuMP.value.(costas; result=sol))
                println("costas:$costas_val")
                differences_val = convert.(Integer,JuMP.value.(differences; result=sol))
                # println("differences:$differences_val")
                for i in 1:n
                    for j in 1:n
                        d = differences_val[i,j]
                        if d > -n+1
                            @printf "%2d " d
                        else 
                            print(" ")
                        end
                    end
                    println()
                end
             end
             println()
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time costas_array(8,true,true,6)

for n in 2:10 
    println("\nn:$n")
    @time costas_array(n,false,true,16)
end
