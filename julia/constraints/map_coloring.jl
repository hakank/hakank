#=

  Map coloring in Julia ConstraintSolver.jl 

  Simple map coloring problem.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=# 

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function map_coloring(problem,print_solutions=true,all_solutions=true)

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
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n, _ = size(problem)
    num_colors = 4
    # println("n:$n num_colors:$num_colors")
    @variable(model, 1 <= x[1:n] <= num_colors, Int)

    for c1 in 1:n, c2 in 1:n 
        if problem[c1,c2] == 1 
            @constraint(model,x[c1] != x[c2])
        end 
    end

    # symmertry breaking
    for c in 1:min(n,num_colors)
        @constraint(model,x[c] <= c)
    end

    # @objective(model,Max,z)

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
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

map_coloring_problems = Dict(
    # Connections between these countries:
    # [belgium, denmark, france, germany, netherlands, luxembourg]
    :1 => resize_matrix([[0, 0, 1, 1, 1, 1],
                         [0, 0, 0, 1, 0, 0],
                         [1, 0, 0, 1, 1, 0],
                         [1, 1, 1, 0, 1, 1],
                         [1, 0, 1, 1, 0, 0],
                         [1, 0, 0, 1, 0, 0]])
       
)

@time map_coloring(map_coloring_problems[:1])

# random instance
function random_map(n=10,lim=0.5)
    m = zeros(Int8,n,n)
    for i in 1:n, j in 1:i-1
       if rand() >= lim
        m[i,j] = 1 
        m[j,i] = 1
       end
    end
    return m
end

for n in 10:20
    println("\nn:$n")
    m = random_map(n,0.5)
    # display(m)
    @time map_coloring(m,true,false)
end
