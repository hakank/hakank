#=

  Global constraint inverse in Julia ConstraintSolver.jl

  From Global Constraint Catalog
  http://www.emn.fr/z-info/sdemasse/gccat/Cinverse.html
  """
  inverse(NODES)

  Enforce each vertex of a digraph to have exactly one predecessor and
  one successor. In addition the following two statements are equivalent:
    - The successor of the ith node is the jth node.
    - The predecessor of the jth node is the ith node.
  """

  Note: This is implemented as "self-assignment", i.e. that
     inverse(X) -> assignment(X,X)
  This means that for each element X[I] either
     - X[I] = I
     or
     - X[I] = J <=> X[J] = I

  Example N=4:

    [1,2,3,4]
    [1,2,4,3]    (X[3]=4, X[4]=3)
    [1,3,2,4]    (X[2]=3, X[3]=2)
    [1,4,3,2]
    [2,1,3,4]
    [2,1,4,3]    (X[1]=2,X[2]=1, X[3]=4,X[4]=3)
    [3,2,1,4]
    [3,4,1,2]
    [4,2,3,1]
    [4,3,2,1]

   Number of solutions for N=1..11:
     [1,2,4,10,26,76,232,764,2620,9496,35696]
   OEIS:
   http://oeis.org/search?q=1%2C2%2C4%2C10%2C26%2C76%2C232%2C764%2C2620%2C9496%2C35696&language=english&go=Search

   http://oeis.org/A000085
   """
   Number of self-inverse permutations on n letters, also known as
   involutions; number of Young tableaux with n cells.
   """

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function inverse_test(n,print_solutions=true,all_solutions=true)

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

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    @variable(model, 1 <= x[1:n] <= n, Int)
    @constraint(model, x in CS.AllDifferentSet())

    inverse(model,x)

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
                println("x:$xx")

            end
        end
        # com = JuMP.backend(model).optimizer.model.inner
        # println("info:$(com.info)")
        # println("start_time:$(com.start_time)")
        # println("solve_time:$(com.solve_time)")
        println()
    else
        println("status:$status")
    end

    return status
end

function test_inverse_range(from,to)
    print_solutions=true
    all_solutions=true
    for n in from:to
        println("n:$n")
        if n > 4
            print_solutions = false
        end
        if n > 9
            all_solutions=false
            print_solutions=true
        end
        @time status = inverse_test(n,print_solutions,all_solutions)
        if status != MOI.OPTIMAL
            break
        end
    end
end


test_inverse_range(1,21)
