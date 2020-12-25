#=

  Set covering deployment in Julia ConstraintSolver.jl

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written "set-covering deployment"
  and abbreviated SCDP for "set covering deployment problem") seeks
  an optimal stationing of troops in a set of regions so that a
  relatively small number of troop units can control a large
  geographic region. ReVelle and Rosing (2000) first described
  this in a study of Emperor Constantine the Great's mobile field
  army placements to secure the Roman Empire.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering_deployment(problem,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> all_solutions,
                                                            "all_optimal_solutions"=>true,
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

    n, _ = size(problem)
    armies = ["Alexandria","Asia Minor","Britain","Byzantium","Gaul","Iberia","Rome","Tunis"]

    # First army
    @variable(model, xs[1:n], Bin)
    # Second army
    @variable(model, ys[1:n], Bin)

    # Objective
    @variable(model, 0 <= z <= 2*n, Int)

    # Constraint 1: There is always an army in a city (+ maybe a backup)
    #          Or rather: Is there a backup, there must be an
    #          an army
    for (x,y) in zip(xs,ys)
        @constraint(model,x>=y)
    end

    # Constraint 2: There should always be an backup army near
    # every city
    for i in 1:n
        @constraint(model, xs[i] + sum([y*m for (y,m) in zip(ys,problem[i,:]) ] ) >= 1)
    end

    # objective: minimize the number of armies
    @constraint(model, z == sum(xs + ys))

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
                xs_val = convert.(Integer,JuMP.value.(xs; result=sol))
                ys_val = convert.(Integer,JuMP.value.(ys; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("z:$z_val")
                println("xs: $xs_val")
                println("ys: $ys_val")
                println("1st army: $([armies[i] for i in 1:n if xs_val[i] > 0])")
                println("2st army: $([armies[i] for i in 1:n if ys_val[i] > 0])")

                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

problem = resize_matrix(
   [[0, 1, 0, 1, 0, 0, 1, 1],
    [1, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 1, 0, 0],
    [1, 1, 0, 0, 0, 0, 1, 0],
    [0, 0, 1, 0, 0, 1, 1, 0],
    [0, 0, 1, 0, 1, 0, 1, 1],
    [1, 0, 0, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 0, 1, 1, 0]]
)

@time set_covering_deployment(problem)
