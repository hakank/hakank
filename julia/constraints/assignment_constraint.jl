#=

  Global constraint assignment in Julia ConstraintSolver.jl

  From the Picat documentation
  """
  assignment(FDVars1,FDVars2): This constraint ensures that FDVars2 is a dual
  assignment of FDVars1, i.e., if the ith element of FDVars1 is j,
  then the jth element of FDVars2 is i.
  """

  Compare with the single array constraint inverse (tested in inverse.jl)

  The number of solutions is - of course - n!

  The number of solutions were X != Y for n=1.. is
  [0,0,2,14,94,644,4808,39556,360260,3619304]
  https://oeis.org/search?q=0%2C0%2C2%2C14%2C94%2C644%2C4808%2C39556%2C360260%2C3619304&sort=&language=english&go=Search
  https://oeis.org/A066052
  """
  A066052: Number of permutations in the symmetric group S_n with order >= 3.
  """

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function assignment_ctr_test(n,print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    @variable(model, 1 <= x[1:n] <= n, Int)
    @variable(model, 1 <= y[1:n] <= n, Int)
    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, y in CS.AllDifferentSet())

    assignment_ctr(model,x, y)


    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_diffs = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols")

        # if print_solutions
        for sol in 1:num_sols
            # println("solution #$sol")
            xx = convert.(Integer,JuMP.value.(x; result=sol))
            yy = convert.(Integer,JuMP.value.(y; result=sol))
            if print_solutions
                println("x:$xx y:$yy")
            end
            # println()
            if xx != yy
            #    println("x:$xx y:$yy")
                num_diffs += 1
            end
        end
        println("num_diffs:$num_diffs")
        # end
        # com = JuMP.backend(model).optimizer.model.inner
        # println("info:$(com.info)")
        # println("start_time:$(com.start_time)")
        # println("solve_time:$(com.solve_time)")
        println()
    else
        println("status:$status")
    end

    return status, num_diffs
end

function test_assignment_ctr_range(from,to)
    print_solutions=true
    all_solutions=true
    num_diffs = []
    for n in from:to
        println("n:$n")
        if n > 4
            print_solutions = false
        end
        if n > 9
            all_solutions=false
            print_solutions=true
        end
        @time status, diffs = assignment_ctr_test(n,print_solutions,all_solutions)
        if status != MOI.OPTIMAL
            break
        else
            push!(num_diffs,diffs)
        end
    end
    println("num_diffs: $num_diffs")
end


test_assignment_ctr_range(1,9)
# assignment_ctr_test(5)
