#=

  Four Islands puzzle (Dell Logic Puzzles) in ConstraintSolver.jl 

  http://brownbuffalo.sourceforge.net/FourIslandsClues.html
  """
  Title: Four Islands
  Author: Humphrey Dudley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1
  
  A tiny nation in the South Pacific contains four islands connected by bridges
  as shown (see below). Each of the four islands (Pwana, Quero, Rayou, and Skern)
  boasts a different primary export (alabaster, bananas, coconuts, and durian
  fruit) and a different tourist attraction (hotel, ice skating rink, jai alai 
  stadium, and koala preserve). Can you find the name, export, and tourist 
  attraction of each island on the map?
  
    N
  W   E     *compass directions
    S
  
  A, B, C, D are the islands
  
  (A) -- (B)
   |      |
   |      |
  (C) -- (D)
  
  
  1. The island noted for its koala preserve is due south of Pwana.
  2. The island with the largest alabaster quarry is due west of Quero.
  3. The island with the resort hotel is due east of the one that exports 
     durian fruit.
  4. Skern and the island with the jai alai stadium are connected by a 
     north-south bridge. 
  5. Rayou and the island that exports bananas are connected by an east-west
     bridge.
  6. The islands noted for the South Pacific's largest ice skating rink and 
     for the jai alai stadium are not connected by a bridge.
  
  Determine: Island island -- Island name -- Export -- Tourist Attraction
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function four_islands(print_solutions=true,all_solutions=true,timeout=6)

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
    n = 4
    A,B,C,D = 1:4
    @variable(model, 1 <= Island[1:n] <= n, Int)
    Pwana, Quero, Rayou, Skern = Island

    @variable(model, 1 <= Export[1:n] <= n, Int)
    Alabaster, Bananas, _Coconuts, DurianFruit = Export  

    @variable(model, 1 <= Attraction[1:n] <= n, Int)
    ResortHotel, IceSkatingRink, JaiAlaiStadium, KoalaPreserve = Attraction  

    @constraint(model, Island in CS.AllDifferentSet())
    @constraint(model, Export in CS.AllDifferentSet())
    @constraint(model, Attraction in CS.AllDifferentSet())


    # 1. The island noted for its koala preserve is due south of Pwana.
    @constraint(model, Pwana == A && KoalaPreserve == C || 
                       Pwana == B && KoalaPreserve == D)


    # 2. The island with the largest alabaster quarry is due west of Quero.
    @constraint(model, Alabaster == A && Quero == B ||  
                       Alabaster == C && Quero == D)

 
    # 3. The island with the resort hotel is due east of the one 
    #    that exports durian fruit.
    @constraint(model, DurianFruit == A && ResortHotel == B  || 
                       DurianFruit == C && ResortHotel == D)

 
    # 4. Skern the island with the jai alai stadium are connected by a 
    #    north-south bridge. 
    @constraint(model, Skern == A && JaiAlaiStadium == C ||
                       Skern == C && JaiAlaiStadium == A || 
                       Skern == B && JaiAlaiStadium == D ||
                       Skern == D && JaiAlaiStadium == B)

                                
    # 5. Rayou/\the island that exports bananas are connected by an 
    #    east-west bridge.
    @constraint(model, (Rayou == A && Bananas == B) || 
                       (Rayou == B && Bananas == A) || 
                       (Rayou == C && Bananas == D) || 
                       (Rayou == D && Bananas == C))


    # 6. The islands noted for the South Pacific's largest ice skating rink 
    #    for the jai alai stadium are not connected by a bridge.
    @constraint(model, IceSkatingRink == A && JaiAlaiStadium == D ||
                       IceSkatingRink == D && JaiAlaiStadium == A ||
                       IceSkatingRink == B && JaiAlaiStadium == C ||
                       IceSkatingRink == C && JaiAlaiStadium == B)


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
                island_val = convert.(Integer,JuMP.value.(Island; result=sol))
                export_val = convert.(Integer,JuMP.value.(Export; result=sol))
                attraction_val = convert.(Integer,JuMP.value.(Attraction; result=sol))
                println("island    :$island_val")
                println("export    :$export_val")
                println("attraction:$attraction_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time four_islands()
