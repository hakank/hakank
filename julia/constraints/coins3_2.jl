#=

  A coin application in Julia ConstraintSolver.jl

  From "The ECLiPSe Book" pages 99f and 234 ff
  The solution in ECLiPSe is at page 236.

  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  There are 4 optimal solutions (8 coins):
    x=[1,2,1,2,1,1]
    x=[1,2,2,1,1,1]
    x=[2,1,1,2,1,1]
    x=[2,1,2,1,1,1]

  This problem was implemented in coins3.jl

  This model implement a related problem:
  Is there a set of coin denominations which make it possible to change
  into 1..99 with less than 8 coins?

  Answer: Yes, there is. For example the following configuration:

  num_coins:7
  variables:[1,2,3,6,12,25,50]
  x:[1,1,1,1,1,1,1]

  Though, it come with a cost of using 7 different coin denominations.

  Also, we can see that there is a set with just 5 different
  denominations for the 8 coin change. However, it requires that
  more than one coin of some of the denominations

  This is done by comment this constraint:
        NumCoins =< 8,

  Result:
    num_coins:8
    variables:[1,2,4,11,33]
    x:[1,1,2,2,2]

Here's the run. As we see it can be done with:
- 8 coins of 5 (and 6) different denominations
- 7 coins of 7 different denominations.

"""
n:5
num_sols:1

solution #1
num_coins:8
coins:[1, 2, 4, 11, 33]
xs:[1, 1, 2, 2, 2]
 15.108249 seconds (24.47 M allocations: 12.097 GiB, 56.45% gc time)

n:6
num_sols:1

solution #1
num_coins:8
coins:[1, 2, 3, 5, 11, 33]
xs:[1, 1, 1, 1, 2, 2]
 21.561126 seconds (34.97 M allocations: 14.951 GiB, 59.19% gc time)

n:7
num_sols:1

solution #1
num_coins:7
coins:[1, 2, 3, 6, 13, 24, 50]
xs:[1, 1, 1, 1, 1, 1, 1]
 15.875412 seconds (16.94 M allocations: 14.988 GiB, 64.24% gc time)
"""

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function coins3_2(n,print_solutions=true,all_solutions=false,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions,
                                                            "logging"=>[],

                                                            "traverse_strategy" => :BFS,
                                                            # "traverse_strategy" => :DFS,
                                                            # "traverse_strategy" => :DBFS,

                                                            "branch_split" => :Smallest,
                                                            # "branch_split" => :Biggest,
                                                            # "branch_split" => :InHalf,

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

    # Which coins should we use?
    # we assume that there are no coin of a denomination larger than 50.
    @variable(model, 1 <= coins[1:n] <= 50, Int)

    # Total number of coins used. Can we do it in 8 or less than 8 coins?
    @variable(model, 1 <= num_coins <= 8, Int)

    # Use either 1 or 2 coins of each denomination
    @variable(model, 1 <= xs[1:n] <= 2, Int)

    @constraint(model, coins in CS.AllDifferent())
    increasing_strict(model, coins) # symmetry breaking

    # Checks that all changes from 1 to 99 can be made.
    table = resize_matrix([ [i,j,i * j] for i in 0:99, j in 0:99])   
    for j in 1:99
       tmp = @variable(model, [1:n], CS.Integers(0:99*99))

       # scalar_product(model,coins,tmp,j) # non-linear constraint
       # Reformulation:
       t = @variable(model, [1:n], CS.Integers(0:99*99)) # the product of coins[i] * tmp[i]
       for i in 1:n 
           @constraint(model, [coins[i], tmp[i], t[i]] in CS.TableSet(table))
           # Ensure that we don't use more coins than in x[i]
           @constraint(model, tmp[i] <= xs[i])
       end
       @constraint(model, j == sum(t))
      
    end
    @constraint(model,num_coins == sum(xs))

    @objective(model,Min,num_coins)

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
                coins_val = convert.(Integer,JuMP.value.(coins; result=sol))
                xs_val = convert.(Integer,JuMP.value.(xs; result=sol))
                num_coins_val = convert.(Integer,JuMP.value.(num_coins; result=sol))
                println("num_coins:$num_coins_val")
                println("coins:$coins_val")
                println("xs:$xs_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

begin
  for n in 1:7
    println("\nn:$n")
    @time status = coins3_2(n ,true,false,125)
    if status == MOI.TIME_LIMIT
      break
    end
  end
end