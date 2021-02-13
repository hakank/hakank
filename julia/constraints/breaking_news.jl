#=

  Breaking News puzzle in ConstraintSolver.jl

  From http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1

  The Daily Galaxy sent its four best reporters 
      (Corey, Jimmy, Lois, and Perry) 
  to different locations 
      (Bayonne, New Hope, Port Charles, and South Amboy) 
  to cover four breaking news events 
      (30-pound baby, blimp launching, skyscraper dedication, and 
       beached whale). 
  Their editor is trying to remember where each of the reporters is. 
  Can you match the name of each reporter with the place he or she 
  was sent, and the event that each covered?

  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, 
     in some order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where 
     the whale was beached, or both.

  Determine: Reporter -- Location -- Story
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function breaking_news(print_solutions=true,all_solutions=true)

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
    n = 4
    reporters = 1:n 
    Corey, Jimmy, Lois, Perry = reporters

    @variable(model, 1 <= locations[1:n] <= n, Int)
    Bayonne, NewHope, PortCharles, SouthAmboy = locations 

    @variable(model, 1 <= news[1:n] <= n, Int)
    Baby, Blimp, Skyscraper, Whale = news 

    @constraint(model, locations in CS.AllDifferentSet())
    @constraint(model, news in CS.AllDifferentSet())

    # 1. The 30-pound baby wasn"t born in South Amboy or New Hope.
    @constraint(model, Baby != SouthAmboy)
    @constraint(model, Baby != NewHope)
    
    # 2. Jimmy didn"t go to Port Charles.
    @constraint(model, Jimmy != PortCharles)
    
    # 3. The blimp launching and the skyscraper dedication were covered, 
    #    in some order, by Lois and the reporter who was sent to 
    #    Port Charles.
    #=
    ( 
        (Blimp == Lois && Skyscraper == PortCharles)
        ||
        (Skyscraper == Lois && Blimp == PortCharles)
    )
    =# 
    @constraint(model, Lois != PortCharles)
    c1 = @variable(model, binary=true)
    c2 = @variable(model, binary=true)
    @constraint(model,c1 := {Blimp == Lois && Skyscraper == PortCharles})
    @constraint(model,c2 := {Skyscraper == Lois && Blimp == PortCharles})
    @constraint(model,c1 + c2 == 1)

    
    # 4. South Amboy was not the site of either the beached whale or the 
    #    skyscraper dedication.
    @constraint(model, SouthAmboy != Whale)
    @constraint(model, SouthAmboy != Skyscraper)
 
    # 5. Bayonne is either the place that Corey went or the place where 
    #    the whale was beached, or both.
    c5 = @variable(model, [1:2],Bin)
    #=
    ( 
        Bayonne == Corey || Bayonne == Whale
    )
    =#
    @constraint(model, c5[1] := {Bayonne == Corey})
    @constraint(model, c5[2] := {Bayonne == Whale})
    @constraint(model, sum(c5) == 1)

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
                locations_val = convert.(Integer,JuMP.value.(locations; result=sol))
                news_val = convert.(Integer,JuMP.value.(news; result=sol))
                println("locations:$locations_val")
                println("news:$news_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time breaking_news()