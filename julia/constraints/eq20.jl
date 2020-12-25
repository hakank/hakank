#=

  Eq10 problem in Julia ConstraintSolver.jl

  Standard benchmark problem.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=# 

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function eq20(print_solutions=true,all_solutions=true)

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
    n = 7
    @variable(model, 0 <= x[1:n] <= 10, Int)
    x0,x1,x2,x3,x4,x5,x6 = x

    @constraint(model, -76706*x0 + 98205*x1 + 23445*x2 + 67921*x3 + 24111*x4 + 
    -48614*x5 + -41906*x6 == 821228)
    @constraint(model, 87059*x0 + -29101*x1 + -5513*x2 + -21219*x3 + 22128*x4 +
    7276*x5 + 57308*x6 == 22167)
    @constraint(model, -60113*x0 + 29475*x1 + 34421*x2 + -76870*x3 + 62646*x4 + 
    29278*x5 + -15212*x6 == 251591)
    @constraint(model, 49149*x0 + 52871*x1 + -7132*x2 + 56728*x3 + -33576*x4 + 
    -49530*x5 + -62089*x6 == 146074)
    @constraint(model, -10343*x0 + 87758*x1 + -11782*x2 + 19346*x3 + 70072*x4 + 
    -36991*x5 + 44529*x6 == 740061)
    @constraint(model, 85176*x0 + -95332*x1 + -1268*x2 + 57898*x3 + 15883*x4 +
    50547*x5 + 83287*x6 == 373854)
    @constraint(model, -85698*x0 + 29958*x1 + 57308*x2 + 48789*x3 + -78219*x4 +
    4657*x5 + 34539*x6 == 249912)
    @constraint(model, -67456*x0 + 84750*x1 + -51553*x2 + 21239*x3 + 81675*x4 + 
    -99395*x5 + -4254*x6 == 277271)
    @constraint(model, 94016*x0 + -82071*x1 + 35961*x2 + 66597*x3 + -30705*x4 + 
    -44404*x5 + -38304*x6 == 25334)
    @constraint(model, -60301*x0 + 31227*x1 + 93951*x2 + 73889*x3 + 81526*x4 + 
    -72702*x5 + 68026*x6 == 1410723)
    @constraint(model, -16835*x0 + 47385*x1 + 97715*x2 + -12640*x3 + 69028*x4 + 
    76212*x5 + -81102*x6 == 1244857)
    @constraint(model, -43277*x0 + 43525*x1 + 92298*x2 + 58630*x3 + 92590*x4 +
    -9372*x5 + -60227*x6 == 1503588)
    @constraint(model, -64919*x0 + 80460*x1 + 90840*x2 + -59624*x3 + -75542*x4 + 
    25145*x5 + -47935*x6 == 18465)
    @constraint(model, -45086*x0 + 51830*x1 + -4578*x2 + 96120*x3 + 21231*x4 +
    97919*x5 + 65651*x6 == 1198280)
    @constraint(model, 85268*x0 + 54180*x1 + -18810*x2 + -48219*x3 + 6013*x4 +
    78169*x5 + -79785*x6 == 90614)
    @constraint(model, 8874*x0 + -58412*x1 + 73947*x2 + 17147*x3 + 62335*x4 +
    16005*x5 + 8632*x6 == 752447)
    @constraint(model, 71202*x0 + -11119*x1 + 73017*x2 + -38875*x3 + -14413*x4 + 
    -29234*x5 + 72370*x6 == 129768)
    @constraint(model, 1671*x0 + -34121*x1 + 10763*x2 + 80609*x3 + 42532*x4 +
    93520*x5 + -33488*x6 == 915683)
    @constraint(model, 51637*x0 + 67761*x1 + 95951*x2 + 3834*x3 + -96722*x4 +
    59190*x5 + 15280*x6 == 533909)
    @constraint(model, -16105*x0 + 62397*x1 + -6704*x2 + 43340*x3 + 95100*x4 + 
    -68610*x5 + 58301*x6 == 876370)
 

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

@time eq20()
