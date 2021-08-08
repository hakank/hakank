#=

  Photo problem in Julia ConstraintSolver.jl 

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for 
  taking a photo. Some of them have preferences next to whom they want to stand:
 
     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.
 
  Obviously, it is impossible to satisfy all preferences. Can you find an alignment that maximizes the number of satisfied preferences?
  """

  Oz solution: 
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]
  
   
  There are 8 solutions:
 
  x = [3, 1, 6, 5, 2, 4, 7]
  x = [3, 1, 7, 5, 2, 4, 6]
  x = [3, 2, 6, 5, 1, 4, 7]
  x = [3, 2, 7, 5, 1, 4, 6]
  x = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
  x = [5, 6, 2, 3, 7, 4, 1]
  x = [5, 7, 1, 3, 6, 4, 2]
  x = [5, 7, 2, 3, 6, 4, 1]


  ConstraintSolver.jl is only fast for the above problem.
  The other two are not so fast...

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function photo_problem(problem,print_solutions=true,all_solutions=true)

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
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            # "activity.decay" => 0.999, # default 0.999
                                                            # "activity.max_probes" => 10, # default, 10
                                                            # "activity.max_confidence_deviation" => 20, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=> 6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n = problem[:n]
    preferences = problem[:preferences]
    n_prefs, _ = size(preferences)
    @variable(model, 1 <= x[1:n] <= n, Int)
    @variable(model, 0 <= z <= n_prefs, Int)

    @constraint(model, x in CS.AllDifferent())

    # Count the number of successful preferences, 
    # i.e when distance is == 1
    diffs = @variable(model, [1:n_prefs], CS.Integers(1:n_prefs))
    bs = @variable(model, [1:n_prefs], Bin) # has diff == 1?
    c = 1 
    for (p1,p2) in eachrow(preferences)
        my_abs(model,x[p1],x[p2], diffs[c])
        @constraint(model,bs[c] := {diffs[c] == 1})
        c += 1
    end
    @constraint(model,z == sum(bs))

    @objective(model,Max,z)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                diffs_val = convert.(Integer,JuMP.value.(diffs; result=sol))
                bs_val = convert.(Integer,JuMP.value.(bs; result=sol))
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                println("z: $z_val")
                println("x: $x_val")
                println("diffs:$diffs_val")
                println("bs   :$bs_val")
                #= 
                c = 1
                for (p1,p2) in eachrow(preferences)
                    println("($p1,$p2) pos: $(x_val[p1]) $(x_val[p2]) diff $(diffs_val[c]) b:$(bs_val[c])")
                    c += 1
                end
                =#
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

photo_problems = Dict(

    #
    # Problem 1 (see above):
    # 1. Betty wants to stand next to Gary and Mary.
    #     1 : 5, 6
    # 2. Chris wants to stand next to Betty and Gary.
    #     2 : 1, 5
    # 3. Fred wants to stand next to Mary and Donald.
    #     4 : 6, 3
    # 4. Paul wants to stand next to Fred and Donald.
    #     7 : 4, 3
    #
    :1  =>  Dict(
        :n => 7,
        :preferences =>
            resize_matrix([[1,5],
             [1,6],
             [2,1],
             [2,5],
             [4,6],
             [4,3],
             [7,4],
             [7,3]]),
        :all_opt_solutions => true
            
    ),
    
    # From http://www.g12.cs.mu.oz.au/minizinc/photo.data2
    :2  =>  Dict(
        :n => 11, 
        :preferences => resize_matrix( 
             [[1,3], 
              [1,5], 
              [1,8], 
              [2,5], 
              [2,9], 
              [3,4], 
              [3,5], 
              [4,1], 
              [4,5], 
              [4,10],
              [5,6], 
              [5,1], 
              [6,1], 
              [6,9], 
              [7,3],
              [7,8], 
              [8,9],
              [8,7], 
              [9,10], 
              [10,11]]),
        :all_opt_solutions => false
    ),
    
    
    # From http://www.ampl.com/NEW/LOGIC/EXAMPLES/photo9.dat
    # (This seems to be a simplified of #2
    :3 =>  Dict(
        :n => 9, 
        :preferences => resize_matrix(
             [[1,3], 
              [1,5], 
              [1,8], 
              [2,5], 
              [2,9], 
              [3,4], 
              [3,5], 
              [4,1], 
              [4,5], 
              [5,1], 
              [5,6], 
              [6,1], 
              [6,9], 
              [7,3],
              [7,8], 
              [8,7],
              [8,9]]
        ),
        :all_opt_solutions => false
    )
)

for p in sort(collect(keys(photo_problems)))
    println("\nProblem $p")
    @time photo_problem(photo_problems[p],true,photo_problems[p][:all_opt_solutions])
    # @time photo_problem(photo_problems[p],true,false)
end

# @time photo_problem(photo_problems[:3],true,false)
