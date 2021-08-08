#=
   Basic model as an template for other models...
=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function alphametic(problem_str="SEND+MORE=MONEY", base=10, print_solutions=true,all_solutions=false,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[], # :true,
                                                            # "logging"=> :true,

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

    problem = split(problem_str,r"[+=-]").|>string
    println("problem: $problem base:$base")
    p_len = length(problem)

    # create the lookup table of (digit -> ix)
    a = join(problem).|>unique
    n = length(a)
    lookup = Dict(zip(a,1:n))
    # println("lookup:",lookup)

    # length of each number 
    lens = problem.|>length 

    @variable(model, 0 <= x[1:n] <= base-1, Int)
    sums = [@variable(model, integer=true,lower_bound=base^(lens[i]-1),upper_bound=(base^lens[i])-1) for i in 1:length(lens)]
    # @variable(model, 1 <= sums[1:length(lens)] <= base^(maximum(lens))-1, Int)

    @constraint(model, x in CS.AllDifferent())

    ix = 1
    for prob in problem
      
      # sum all the digits with proper exponents to a number
      this_len = length(prob)
      ss = [(base^i) * x[lookup[prob[this_len - i]]] for i in (this_len-1):-1:0]
      @constraint(model,sums[ix] == sum(ss))

      # leading digits must be > 0
      @constraint(model,x[lookup[prob[1]]] > 0)

      ix += 1

    end

    # the last number (RHS) is the sum of the previous numbers (LHS)
    @constraint(model, sum([sums[i] for i in 1:p_len-1]) == sums[end])

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
                println("x:$x_val")
                for c in a
                  println("$c: $(x_val[lookup[c]])")
                end
            end
        end
        println()
    else
        println("status:$status")
    end

    return status, num_sols
end

problems = [
  "SEND+MORE=MONEY", 
  "SEND+MOST=MONEY", "VINGT+CINQ+CINQ=TRENTE",
  "EIN+EIN+EIN+EIN=VIER", 
  "DONALD+GERALD=ROBERT",
  "SATURN+URANUS+NEPTUNE+PLUTO+PLANETS",  # this is hard
  "WRONG+WRONG=RIGHT"
]

function run_problems(base=10)
  for problem in problems   
    # @time alphametic(problem,10,true,true,6)
    @time alphametic(problem,base,true,false,116)
    println()
  end

end

#=
if length(ARGS) > 0
  problem = ARGS[1]
  base = 10 
  if length(ARGS) > 1
    base = parse(Int,ARGS[2])
  end
  if problem == "TEST" || problem == "test"
    run_problems(base)
  else 
    alphametic(problem)
  end
else 
  run_problems()
end
=#

run_problems()