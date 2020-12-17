#=

  Traffic lights problem in Julia ConstraintSolver.jl

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of the traffic
  lights are for the vehicles and can be represented by the variables V1 to V4 with domains
  {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are
  for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.

  The constraints on these variables can be modelled by quaternary constraints on
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.

  It would be interesting to consider other types of junction (e.g. five roads
  intersecting) as well as modelling the evolution over time of the traffic light sequence.
  ...

  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.

  (V1,P1,V2,P2,V3,P3,V4,P4) =
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}


  The problem has relative few constraints, but each is very tight. Local propagation
  appears to be rather ineffective on this problem.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

function traffic_lights(print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))



    n=4
    r,ry,g,y = 1:n
    d = Dict(1=>"r",2=>"ry",3=>"g",4=>"y")
    # allowed light combinations
    allowed = [r r g g;
               ry r y r;
               g g r r;
               y r ry r
              ]

    @variable(model, v[1:n],  CS.Integers([r,ry,g,y]))
    @variable(model, p[1:n],  CS.Integers([r,ry,g,y]))

    for i in 1:n, j in 1:n
        if j == (1+i) % n
            @constraint(model, [v[i], p[i], v[j], p[j]] in CS.TableSet(allowed))
        end
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
                vv = convert.(Integer,JuMP.value.(v; result=sol))
                pp = convert.(Integer,JuMP.value.(p; result=sol))
                # println("v:$vv  p:$pp")
                for i in 1:n
                print(d[vv[i]]," ", d[pp[i]], " ")
                end
                println()
            end
            println()
        end
    else
        println("status:$status")
    end

    return status
end

@time traffic_lights()
