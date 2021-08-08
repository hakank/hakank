#=

  TSP (traveling salesperson problem) in Julia ConstraintSolver.jl

  Inspired by the code from lecture notes
  Ulf Nilsson: Transparencies for the course TDDD08 Logic
  Programming, page 6f
  http://www.ida.liu.se/~TDDD08/misc/ulfni.slides/oh10.pdf


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function tsp(problem,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>timeout,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))


    problem = resize_matrix(problem)
    n, _ = size(problem)
    min_dist, max_dist = extrema(problem)
    println("min_dist:$min_dist max_dist:$max_dist (max_dist*n: $(max_dist*n))")

    @variable(model, 1 <= cities[1:n] <= n, Int)
    @variable(model, 1 <= path[1:n] <= n, Int)
    @variable(model, min_dist <= costs[1:n] <= max_dist, Int)
    @variable(model, 0 <= cost <= max_dist*n, Int)

    @constraint(model, cities in CS.AllDifferent())
    @constraint(model, path in CS.AllDifferent())
    circuit_path(model,cities,path)
    @constraint(model, cities[n] == 1)

    for i in 1:n
        my_element(model,cities[i],problem[i,:],costs[i])
    end
    @constraint(model, cost == sum(costs))

    # This is too slow!
    # Instead we check all solutions "manually" below and pick the best.
    # @objective(model,Min, cost)

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        min_cost = 99999999
        min_cities = []
        min_path = []
        min_costs = []
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                citiesx = convert.(Integer,JuMP.value.(cities; result=sol))
                costsx = convert.(Integer,JuMP.value.(costs; result=sol))
                costx = convert.(Integer,JuMP.value.(cost; result=sol))
                pathx = convert.(Integer,JuMP.value.(path; result=sol))
                if costx < min_cost
                    println("solution #$sol")
                    println("cities:$citiesx")
                    println("path  :$pathx")
                    println("costs :$costsx")
                    println("cost  :$costx")
                    println()
                    min_cost = costx
                    min_cities = citiesx
                    min_costs = costsx
                end

            end
        end
    else
        println("status:$status")
    end

    return status
end

tsp_problems = Dict(

#
# Problem from Nilsson cited above.
#
:nilsson =>
       [[ 0, 4, 8,10, 7,14,15],
        [ 4, 0, 7, 7,10,12, 5],
        [ 8, 7, 0, 4, 6, 8,10],
        [10, 7, 4, 0, 2, 5, 8],
        [ 7,10, 6, 2, 0, 6, 7],
        [14,12, 8, 5, 6, 0, 5],
        [15, 5,10, 8, 7, 5, 0]],

#
# This problem is from the SICStus example
# ./library/clpfd/examples/tsp.pl
# The "chip" examples
#
:chip =>
       [[0,205,677,581,461,878,345],
        [205,0,882,427,390,1105,540],
        [677,882,0,619,316,201,470],
        [581,427,619,0,412,592,570],
        [461,390,316,412,0,517,190],
        [878,1105,201,592,517,0,691],
        [345,540,470,570,190,691,0]],


# This problem is from the SICStus example
# ./library/clpfd/examples/tsp.pl
# The "ilog" examples
#
:ilog =>
       [[2,4,4,1,9,2,4,4,1,9],
        [2,9,5,5,5,2,9,5,5,5],
        [1,5,2,3,3,1,5,2,3,3],
        [2,6,8,9,5,2,6,8,9,5],
        [3,7,1,6,4,3,7,1,6,4],
        [1,2,4,1,7,1,2,4,1,7],
        [3,5,2,7,6,3,5,2,7,6],
        [2,7,9,5,5,2,7,9,5,5],
        [3,9,7,3,4,3,9,7,3,4],
        [4,1,5,9,2,4,1,5,9,2]],

# This problem is from
# GLPK:s example tsp.mod
# (via http://www.hakank.org/minizinc/tsp.mzn)
# """
# These data correspond to the symmetric instance ulysses16 from:
# Reinelt, G.: TSPLIB - A travelling salesman problem library.
# ORSA-Journal of the Computing 3 (1991) 376-84;
# http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib
#
# The optimal solution is 6859
# """
:glpk =>
  [[0,509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150],
   [509,0,126,474,1526,1226,1133,532,1449,1122,2789,958,941,978,1127,542],
   [501,126,0,541,1516,1184,1084,536,1371,1045,2728,913,904,946,1115,499],
   [312,474,541,0,1157,980,919,271,1333,1029,2553,751,704,720,783,455],
   [1019,1526,1516,1157,0,478,583,996,858,855,1504,677,651,600,401,1033],
   [736,1226,1184,980,478,0,115,740,470,379,1581,271,289,261,308,687],
   [656,1133,1084,919,583,115,0,667,455,288,1661,177,216,207,343,592],
   [60,532,536,271,996,740,667,0,1066,759,2320,493,454,479,598,206],
   [1039,1449,1371,1333,858,470,455,1066,0,328,1387,591,650,656,776,933],
   [726,1122,1045,1029,855,379,288,759,328,0,1697,333,400,427,622,610],
   [2314,2789,2728,2553,1504,1581,1661,2320,1387,1697,0,1838,1868,1841,1789,2248],
   [479,958,913,751,677,271,177,493,591,333,1838,0,68,105,336,417],
   [448,941,904,704,651,289,216,454,650,400,1868,68,0,52,287,406],
   [479,978,946,720,600,261,207,479,656,427,1841,105,52,0,237,449],
   [619,1127,1115,783,401,308,343,598,776,622,1789,336,287,237,0,636],
   [150,542,499,455,1033,687,592,206,933,610,2248,417,406,449,636,0]
  ],

)

print_solutions = true
all_solutions = true
timeout = 6
# #=
solved = []
for problem in sort(collect(keys(tsp_problems)))
    println("\nproblem $problem")
    @time status = tsp(tsp_problems[problem],print_solutions,all_solutions,timeout)
    if status == MOI.OPTIMAL
        push!(solved,problem)
    end
end
# =#
# @time tsp(tsp_problems[:nilsson],print_solutions,all_solutions)
println("solved: $solved")
