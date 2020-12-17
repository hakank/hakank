#=

  Just forgotten puzzle (Enigma 1517) in Julia ConstraintSolver.jl

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.

  Joe was furious when he forgot one of his bank account numbers.
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:

      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function just_forgotten(print_solutions=true,all_solutions=true)

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
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

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
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    a = resize_matrix([[9,4,6,2,1,5,7,8,3,0],
                       [8,6,0,4,3,9,1,2,5,7],
                       [1,6,4,0,2,9,7,8,5,3],
                       [6,8,2,4,3,1,9,0,7,5]])
    rows,cols = size(a)
    @variable(model, 0 <= x[1:cols] <= 9, Int)
    @constraint(model, x in CS.AllDifferentSet())


    for row in eachrow(a)
        # These two versions don't work in ConstraintSolver.jl v0.6.0
        # @constraint(model, sum(x .== row) == 4)
        # @constraint(model, sum([x[i] == row[i] for i in 1:cols]) == 4)

        # booleans to the rescue!
        b = @variable(model, [1:cols], Bin)
        for i in 1:cols
            @constraint(model,b[i] := {x[i] == row[i]})
        end
        @constraint(model, sum(b) == 4)
    end

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
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

println("Proving unicity")
@time just_forgotten(true,true)
println("\n'First' solution")
@time just_forgotten(true,false)
