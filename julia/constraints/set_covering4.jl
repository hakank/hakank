#=

  Set covering and set partition in Julia ConstraintSolver.jl

  Example from Lundgren, Ronnqvist, Varbrand "Optimeringslora", page 408.
  [This is a Swedish book about Operational Research.]

  We want to minimize the cost of the alternatives which covers all the
  objects, i.e. all objects must be choosen. The requirement is than an object
  may be selected _exactly_ once.

  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7

  The problem has a unique solution of z = 49 where alternatives
  3, 5, and 9 is selected.

  If we, however, allow that an object is selected more than one time,
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives 4, 8, and 10 is selected, where object 5 is
  selected twice (alt. 4 and 8). It's an unique solution as well.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function set_covering4(problem,type="",print_solutions=true,all_solutions=true)

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
                                                            # "activity.max_probes" => 100, # default, 10
                                                            # "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    println("Type:$type")
    costs = problem[:costs]
    alternatives = problem[:alternatives]

    num_alternatives, num_objects = size(alternatives)

    # Which alternative to select
    @variable(model, x[1:num_alternatives], Bin)
    @variable(model, 0 <= min_val <= num_alternatives*maximum(costs), Int)

    for j in 1:num_objects
        s = @variable(model, [1:1], CS.Integers(0:num_alternatives*sum(alternatives[:,j])))
        @constraint(model, s[1] == sum(x.*alternatives[:,j]))
        if type == "set_partition"
            # all objects must be covered _exactly_ once 
            # (set partition)
            @constraint(model, s[1] == 1)
        else
            # variant: all objects must be covered _at least_ once
            # (set covering)
            @constraint(model, s[1] >= 1)
        end
    end
    
    scalar_product(model,costs,x,min_val)
    
    @objective(model,Min,min_val)
    
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
                min_val_val = convert.(Integer,JuMP.value.(min_val; result=sol))
                println("min_val:$min_val_val")
                println("x:$x_val")
                println("assignments:$([i for i in 1:num_alternatives if x_val[i] == 1])")
            end
        end
    else
        println("status:$status")
    end

    return status
end

problem = Dict(
    :costs => [9, 16, 18, 13, 15, 19, 15, 17, 16, 15],
    :alternatives => resize_matrix([
        [1,0,0,0,0,1,0,0],   # alternative 1
        [0,1,0,0,0,1,0,1],   # alternative 2
        [1,0,0,1,0,0,1,0],   # alternative 3
        [0,1,1,0,1,0,0,0],   # alternative 4
        [0,1,0,0,1,0,0,0],   # alternative 5
        [0,1,1,0,0,0,0,0],   # alternative 6
        [0,1,1,1,0,0,0,0],   # alternative 7
        [0,0,0,1,1,0,0,1],   # alternative 8
        [0,0,1,0,0,1,0,1],   # alternative 9
        [1,0,0,0,0,1,1,0]]  # alternative 10
                                   )
)

@time set_covering4(problem,"set_partition",true,true)
println()
@time set_covering4(problem,"set_covering",true,true)
