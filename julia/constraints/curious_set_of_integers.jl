#=

  Curious set of integers in Julia ConstraintSolver.jl 

  Martin Gardner (February 1967):
  """
  The integers 1,3,8, and 120 form a set with a remarkable property:
  the product of any two integers is one less than a perfect square. 
  Find a fifth number that can be added to the set without destroying 
  this property.
  """

  Solution: The number is 0.
 
  There are however other sets of five numbers with this property.
  Here are the one in the range of 0..10000:

  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function curious_set_of_integers(print_solutions=true,all_solutions=false,timeout=6)

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
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n = 5
    dom_max = 120
    @variable(model, 0 <= x[1:n] <= dom_max, Int)
    
    @constraint(model, x in CS.AllDifferent())
    increasing_strict(model, x)

    # It's faster without these hints
    # @variable(model, 1 <= ixs[1:n] <= n, Int) # indices in X
    # @constraint(model, ixs in CS.AllDifferent())
    # my_element(model,ixs[1],x,1)
    # my_element(model,ixs[2],x,3)
    # my_element(model,ixs[3],x,8)
    # my_element(model,ixs[4],x,120)


    table = resize_matrix([ [i,j,i * j] for i in 0:dom_max, j in 0:dom_max])
    ps = []
    for i in 1:n, j in 1:i-1
        p = @variable(model, integer=true, lower_bound=0, upper_bound=dom_max)
        # @constraint(model, p*p -1 == x[i]*x[j]) # non-linear constraints!

        p_sq = @variable(model, integer=true, lower_bound=0, upper_bound=dom_max^2)
        @constraint(model, [p, p, p_sq] in CS.TableSet(table))

        xij_sq = @variable(model, integer=true, lower_bound=0, upper_bound=dom_max^2)
        @constraint(model, [x[i], x[j], xij_sq] in CS.TableSet(table))

        @constraint(model,p_sq-1 == xij_sq)

        push!(ps,p)
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

@time curious_set_of_integers(true,false)
