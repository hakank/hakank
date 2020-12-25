#=

  Zoo, buses and kids problem in Julia ConstraintSolver.jl 

  https://dmcommunity.org/challenge/challenge-july-2018/
  """
  300 kids need to travel to the London zoo. The school may rent 40 seats and 
  30 seats buses for 500 and 400 £. How many buses of each to minimize cost?
  """

  Also, see Alex Fleisher
  "Optimization : simply do more with less, zoo, buses and kids (Part2, python, java, C++…)"
  ""
  https://alexfleischer-84755.medium.com/optimization-simply-do-more-with-less-zoo-buses-and-kids-part2-python-java-c-cc04558e49b5


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function zoo(problem,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>all_solutions, 
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

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    num_kids  = problem[:num_kids]
    num_seats = problem[:num_seats]
    price     = problem[:price]
    n         = problem[:n]
                                     
    @variable(model, 1 <= x[1:n] <= 100, Int)
    @variable(model, 0 <= z <= sum(price.*num_seats), Int)

    scalar_product(model, x,num_seats,:(>=), num_kids)
    @constraint(model, z == sum(x.*price))
    
    @objective(model,Min,z)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("x:$x_val")
                println("z:$z_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

zoo_problem = Dict(
   :num_kids  => 300,
   :num_seats => [30,40],
   :price     => [400,500],
   :n         => 2,

)

@time zoo(zoo_problem)
