#=

  Ski assignment in Julia ConstraintSolver.jl

  From
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008, Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort.
  They'll let you ski all winter for free in exchange for helping their ski rental
  shop with an algorithm to assign skis to skiers. Ideally, each skier should
  obtain a pair of skis whose height matches his or her own height exactly.
  Unfortunately, this is generally not possible. We define the disparity between
  a skier and his or her skis to be the absolute value of the difference between
  the height of the skier and the pair of skis. Our objective is to find an
  assignment of skis to skiers that minimizes the sum of the disparities.
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function ski_assignment(print_solutions=true,all_solutions=false, all_optimal_solutions=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>all_optimal_solutions,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    ski_heights   =  [1, 2, 5, 7, 13, 21]
    skier_heights =  [3, 4, 7, 11, 18]

    num_skis = length(ski_heights)
    num_skiers = length(skier_heights)

    @variable(model, 1 <= x[1:num_skiers] <= num_skis, Int)
    @constraint(model, x in CS.AllDifferentSet())

    # minimize the differences of ski height and skier's height (z)
    t = 100
    @variable(model, 0 <= z <= t, Int)
    @variable(model, sxi[1:num_skiers], CS.Integers(ski_heights))
    @variable(model, 0 <= abs_diff[1:num_skiers] <= t, Int)

    # We must use element here...
    for i in 1:num_skiers
        # find the ski height for skier i
        my_element(model,x[i],ski_heights,sxi[i])
        # the difference beteen ski height and skier's height
        # my_abs(model,sxi[i],skier_heights[i],abs_diff[i])
        my_abs(model,sxi[i]-skier_heights[i],abs_diff[i])
    end
    @constraint(model,z==sum(abs_diff))

    @objective(model,Min, z)

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
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                zz = convert.(Integer,JuMP.value.(z; result=sol))
                sxix = convert.(Integer,JuMP.value.(sxi; result=sol))
                abs_diffx = convert.(Integer,JuMP.value.(abs_diff; result=sol))
                println("x:$xx  z:$zz")
                println("sxi:$sxix")
                println("abs_diff:$abs_diffx")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time ski_assignment()
