#=
  Steiner triplets in ConstraintSolver.jl

  http://www.probp.com/examples/clpset/steiner.pl
  """
  The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements 
  in {1,2,...,n} such that each set contains three elements and any two 
  sets have at most one element in common.

  For example, the following shows a solution for size n=7:

      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

  Problem taken from:
  C. Gervet: Interval Propagation to Reason about Sets: Definition and 
             Implementation of a PracticalLanguage,  
             Constraints, An International Journal, vol.1, pp.191-246, 1997.
  """


  Note: This model uses triplets of integers (in comparison to 
        binary representation in steiner.jl) and is faster!

  This model manage to solve n=15 (timeout 16s)
n:3 nb:1
[1 2 1]
  0.159783 seconds (428.18 k allocations: 25.673 MiB, 99.24% compilation time)

n:7 nb:7
[1 4 6; 1 3 5; 3 4 7; 1 2 7; 2 5 6; 4 5 7; 4 2 2]
  0.067136 seconds (503.62 k allocations: 26.409 MiB)

n:9 nb:12
[1 4 6; 4 5 7; 5 6 8; 3 6 8; 4 8 9; 1 6 9; 3 4 9; 6 7 9; 1 7 8; 3 7 9; 1 3 5; 2 2 2]
  2.441683 seconds (11.63 M allocations: 495.798 MiB, 44.58% gc time)

n:13 nb:26
[1 3 11; 8 10 12; 1 2 5; 2 10 11; 5 7 10; 2 7 9; 3 6 13; 1 8 9; 3 5 12; 2 4 13; 7 10 12; 8 11 13; 9 10 13; 3 9 10; 7 11 13; 3 7 8; 1 6 7; 5 6 8; 4 11 12; 6 10 11; 6 9 12; 5 9 11; 1 12 13; 2 8 12; 2 3 6; 4 1 4]
  4.297591 seconds (17.44 M allocations: 869.541 MiB, 33.34% gc time)

n:15 nb:35
[1 2 8; 7 10 15; 5 8 15; 3 6 9; 2 3 5; 3 4 8; 1 6 7; 1 12 15; 6 7 14; 1 13 14; 6 11 15; 4 9 15; 3 13 15; 2 14 15; 8 11 12; 8 10 14; 1 4 5; 1 3 10; 2 12 13; 7 8 13; 2 10 11; 6 10 12; 5 11 14; 5 10 13; 2 7 9; 5 7 12; 1 11 12; 5 6 8; 4 12 14; 4 11 13; 4 7 10; 3 12 14; 3 7 11; 2 4 6; 9 1 9]
  6.824711 seconds (29.03 M allocations: 1.556 GiB, 21.72% gc time)

n:19 nb:57
status:TIME_LIMIT
 16.978271 seconds (53.21 M allocations: 3.464 GiB, 25.02% gc time)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function steiner(n=7,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            # "branch_strategy" => :IMPS, # default
                                                            "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 1, # default, 10
                                                            "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    if !(n % 6 == 1 || n % 6 == 3)
        println("n must be (1|3) modulo 6")
        return
    end

    nb = round(Int,(n * (n-1)) / 6) # number of sets
    println("n:$n nb:$nb")

    @variable(model, 1 <= x[1:nb,1:3] <= n, Int)
    @constraint(model, x[1,1] == 1) # symmetry breaking

    # atmost 1 element in common
    for i in 1:nb, j in i+1:nb
        increasing_strict(model, x[i,:])
        b = @variable(model, [1:3,1:3], Bin)
        for p in 1:3, q in 1:3 
            @constraint(model, b[p,q] := {x[i,p] == x[j,q]} )
        end
        @constraint(model, sum(b[:]) <= 1)
    end
    

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println(x_val)
            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

# @time steiner(7,true,false)
for n in [i for i in 3:20 if (i % 6 == 1 || i % 6 == 3)]
    @time status, num_sols = steiner(n,true,false,16)
    println()
    if status == MOI.TIME_LIMIT 
        break
    end 
end