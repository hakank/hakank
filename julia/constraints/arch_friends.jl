#=

  Arch Friends puzzle (Dells Logic Puzzles) in Julia ConstraintSolver.jl 


  From http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four
  shoe purchases to her friend Aurora. Aurora just loves the four
  different kinds of shoes that Harriet bought 
     (ecru espadrilles,fuchsia flats, purple pumps, and suede sandals),
  but Harriet can't recall at which different store 
     (Foot Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) 
  she got each pair. Can you help these two figure out the order in
  which Harriet bought each pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not
     Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede
     sandals.

  Determine: Order - Shoes - Store 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function arch_friends(print_solutions=true,all_solutions=true)

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

    @variable(model, 1 <= shoes[1:n] <= n, Int)
    EcruEspadrilles, FuchsiaFlats, PurplePumps, SuedeSandals = shoes

    @variable(model, 1 <= stores[1:n] <= n, Int)
    FootFarm, HeelsInAHandcart, TheShoePalace, Tootsies = stores

    @constraint(model, shoes in CS.AllDifferent())
    @constraint(model, stores in CS.AllDifferent())

    # 1. Harriet bought fuchsia flats at Heels in a Handcart.
    @constraint(model, FuchsiaFlats == HeelsInAHandcart)

    # 2. The store she visited just after buying her purple pumps was not
    #    Tootsies.
    @constraint(model, PurplePumps + 1 != Tootsies)

    # 3. The Foot Farm was Harriet's second stop.
    @constraint(model, FootFarm == 2)

    # 4. Two stops after leaving The Shoe Place, Harriet bought her suede
    # sandals.
    @constraint(model, TheShoePalace + 2 == SuedeSandals)

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
                shoes_val = convert.(Integer,JuMP.value.(shoes; result=sol))
                stores_val = convert.(Integer,JuMP.value.(stores; result=sol))
                println("shoes :$shoes_val")
                println("stores:$stores_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

@time arch_friends()
