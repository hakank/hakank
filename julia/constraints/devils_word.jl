#=

  Devil's word in ConstraintSolver.jl

  Translate each character in a word to ASCII value and then try
  to sum its values (either positive or negative) to a total.
  
  E.g. "hakankjellerstrand" and total 666 gives 359 solutions.
  Here is the first:
  +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100
 
  Also, see http://www.hakank.org/data_snooping/666.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function devils_word(s,total=666,print_solutions=true,all_solutions=true,timeout=6)

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

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,

                                                            ))

    println("s:$s")
    a = [Int(c[1]) for c in s]
    println("a:$a")
    println("total:$total")
    len = length(a)
    @variable(model, x[1:len], CS.Integers([-1,1]))

    @constraint(model, sum(x.*a) == total)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                println([a[i]*x_val[i] for i in 1:len])
                println(join([string(x_val[i]==1 ? "+" : "-",s[i]) for i in 1:len],""))
                println()

            end
        end
    else
        println("status:$status")
    end
    println("num_sols:$num_sols")
    return status, num_sols
end



total = 666
tests = ["hakankjellerstrand","juliaprogramming","juliaisfun"]
for s in tests 
    @time devils_word(s,total)
    println()
end
