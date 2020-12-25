#=

  Global constraint global contiguity regular in Julia ConstraintSolver.jl 

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
  In addition, all variables assigned to value 1 appear contiguously.
  
  Example:
  (<0,1,1,0>)
  
  The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
  no more than one group of contiguous 1.
  """

  Here's a variant of the global constraint global contiguity
  using (a decomposition of) the regular constraint.


  For n=0:13 the number of solutions is 
  [1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79]

  Which is the (or rather one of the possible) OEIS sequence https://oeis.org/A000124
  """
  Central polygonal numbers (the Lazy Caterer's sequence): n(n+1)/2 + 1; or, 
  maximal number of pieces formed when slicing a pancake with n cuts.
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function global_contiguity_test(n,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
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

    @variable(model, x[1:n], Bin)

    y = global_contiguity_regular(model, x)


    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                y_val = convert.(Integer,JuMP.value.(y; result=sol))
                println("x:$x_val")
                println("y:$y_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end


sols = []
for n in 0:13
    global sols
    @time status, num_sols = global_contiguity_test(n,true,true)
    if status == MOI.OPTIMAL
        push!(sols,num_sols)
    end
end
println("\nsols:$sols")


# @time global_contiguity_test(10,true,true)