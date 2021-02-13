#=
  Number of days problem (knapsack) ConstraintSolver.jl

  From Nathan Brixius
  "Solving a Knapsack problem with Solver Foundation and LINQ"
  http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
 """
  Let's say I have this list of days and prices:

    List<ReservationPrice> prices = new List<ReservationPrice>(); 
    prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 

  What I would like to able to do now is: give me the best price 
  from the list based on a number of days.

  So if ask for 3 days the best price from the list is from child one 
  (1000) and two (1200), but there are of course different combinations. 
  How would an algorithm that found the best price from this list 
  look like ?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function number_of_days(days,cost,day,print_solutions=true,all_solutions=true,timeout=6)

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

                                                            "time_limit"=>timeout,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    num_days = length(days)
    max_days = sum(days)
    max_cost = sum(cost)
    @variable(model, x[1:num_days], Bin)
    @variable(model, 1 <= total_cost <= max_cost,Int)

    @constraint(model, day == sum(days .* x))
    @constraint(model, total_cost == sum(cost .* x))


    @objective(model,Min,total_cost)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                total_cost_val = convert.(Integer,JuMP.value.(total_cost; result=sol))
                println("x:$x_val total_cost:$total_cost_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

days = [1,2,3,4,7]
cost = [1000,1200,2500,3100,4000]

for day in 1:sum(days)
    println("\nday:$day")
    @time number_of_days(days,cost,day)
end
