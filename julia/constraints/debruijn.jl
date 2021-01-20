#=
  de Bruijn sequence in Julia ConstraintSolver

  Implementation of de Bruijn sequences in Comet, both "classical"
  and "arbitrary".

  Compare with the the web based programs:
    http://www.hakank.org/comb/debruijn.cgi
    http://www.hakank.org/comb/debruijn_arb.cgi

  For Base = 2, N = 3, M = 8 there are 2 solutions:
    x : [](0, 1, 3, 7, 6, 5, 2, 4)
    bincode : [0, 0, 0, 1, 1, 1, 0, 1]

    x : [](0, 1, 2, 5, 3, 7, 6, 4)
    bincode : [0, 0, 0, 1, 0, 1, 1, 1]

  There are two symmetry breaking constraints:
  - if possible, we require that the occurrences of number are the same,
    i.e if M mod Base == 0.
  - the sequence X always start with the minumum value (0).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function debruijn(base=2,n=3,m=base^n,all_solutions=true,print_solutions=true,timeout=6)
    println("base:$base n:$n m:$m")
    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>timeout,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    # x: list of the integers to use which are converted to
    #    'base-ary' numbers below
    @variable(model, 0 <= x[1:m] <= (base^n)-1, Int)

    # binary: the matrix of the "fully expanded" integers in X
    @variable(model, 0 <= binary[1:m,1:n] <= base-1, Int)

    # number of occurrences for each number
    @variable(model, 0 <= gcc[1:base] <= m, Int)

    @constraint(model, x in CS.AllDifferentSet())

    # Connect x and binary
    for i in 1:m
        # convert the integer to base-ary representation
        to_num(model, [binary[i,j] for j in 1:n], base, x[i])
    end

    # The de Bruijn criterion: Connect one element to the next...
    for i in 2:m, j in 2:n
        @constraint(model,binary[i-1, j] == binary[i, j-1])
    end

    # ... and around the corner.
    for j in 2:n
        @constraint(model, binary[m, j] == binary[1, j-1])
    end


    # bin_code: The de Bruijn sequence, i.e. the first
    #           elements in each row in Binary
    #           Note: this is just a slice.
    bin_code = [binary[i,1] for i in 1:m]

    # GCC: Count the number of different elements in the bin code
    # (poor man's global cardinarlity count...)
    for i in 1:base
        count_ctr(model,bin_code, :(==), i-1, gcc[i])
    end

    # If possible, we require that the occurrences of number are the same.
    # Gcc
    if m % base == 0
        for i in 2:base
            @constraint(model,gcc[i] == gcc[i-1])
            @constraint(model,gcc[i] == round(Int,m / base))
        end
    end

    # symmetry breaking
    @constraint(model, x[1] .<= x)

    println("solve")
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
                binaryx = convert.(Integer,JuMP.value.(binary; result=sol))
                gccx = convert.(Integer,JuMP.value.(gcc; result=sol))
                if base < 10
                    bin_codex = join([binaryx[i,1] for i in 1:m],"")
                else
                    bin_codex = [binaryx[i,1] for i in 1:m]
                end

                println("x: $xx")
                println("binary:")
                for row in eachrow(binaryx)
                    println(row)
                end
                println("bin_code: $bin_codex")
                println("gcc: $gccx")
                println()

            end
            println("num_sols: $num_sols\n")
        end
    else
        println("status: $status")
    end

    return status
end


base=2
n=3
m=base^n
all_solutions= true
print_solutions=true
@time debruijn(base,n,m,all_solutions,print_solutions)

# Test a non standard version: m = 27 (instead of 2^5=32)
# It should be 2000 solutions. And it is, but it takes ≈ 29s!
base = 2
n = 5
m = 27
@time debruijn(base,n,m,all_solutions,false,30)
