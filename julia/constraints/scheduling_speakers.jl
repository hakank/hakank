#=

  Scheduling speakers in Julia ConstraintSolver.jl

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function scheduling_speakers(available_slots,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

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

    n = length(available_slots)
    @variable(model, 1 <= xs[1:n] <= n, Int)

    @constraint(model, xs in CS.AllDifferent())

    for (x,a) in zip(xs,available_slots)
        len = length(a)
        b = @variable(model,[1:len],Bin)
        for i in 1:len
            @constraint(model,b[i] := {a[i] == x})
        end
        @constraint(model,sum(b) >= 1)

        # This version is too slow for showin all (2) solutions:
        # ix = @variable(model,[1:len],CS.Integers(1:len))
        # my_element(model,ix[1],a,x)
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
                # println("solution #$sol")
                xs_val = convert.(Integer,JuMP.value.(xs; result=sol))
                println("xs:$xs_val")
            end
        end
    else
        println("status:$status")
    end

    return status
end

available_slots = [
                       # Reasoning:
         [3,4,5,6],    # 2) the only one with 6 after speaker F -> 1
         [3,4],        # 5) 3 or 4
         [2,3,4,5],    # 3) only with 5 after F -> 1 and A -> 6
         [2,3,4],      # 4) only with 2 after C -> 5 and F -> 1
         [3,4],        # 5) 3 or 4
         [1,2,3,4,5,6] # 1) the only with 1

]
@time scheduling_speakers(available_slots,true,true)
