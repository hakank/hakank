#=

  Nontransitive dice in ConstraintSolver.jl 

  From 
  http://en.wikipedia.org/wiki/Nontransitive_dice
  """
  A set of nontransitive dice is a set of dice for which the relation 
  "is more likely to roll a higher number" is not transitive. See also 
  intransitivity.
  
  This situation is similar to that in the game Rock, Paper, Scissors, 
  in which each element has an advantage over one choice and a 
  disadvantage to the other.
  """

  From the Wikipedia page:
    A > B = 5/9   B > C = 5/9  C > A = 5/9
     [2,2,4,4,9,9] die A
     [1,1,6,6,8,8] die B
     [3,3,5,5,7,7] die C

  Here are some examples:

  * The minimum die value for a 3 x 6 dice problem is 4, e.g.
    dice:
    [1, 1, 3, 4, 4, 4]
    [3, 3, 3, 3, 3, 4]
    [2, 2, 2, 4, 4, 4]
    competitions:
    1 vs 2:[15, 13]
    2 vs 3:[18, 15]
    3 vs 1:[15, 12]
    max_dice_val:4
    max_win:18

  * Here's an example of a 6 x 6 dice configuration:
  dice:
    [1, 5, 5, 6, 6, 6]
    [5, 5, 5, 5, 6, 6]
    [4, 4, 5, 6, 6, 6]
    [3, 3, 6, 6, 6, 6]
    [2, 2, 6, 6, 6, 6]
    [1, 4, 6, 6, 6, 6]
    competitions:
    1 vs 2:[12, 10]
    2 vs 3:[14, 12]
    3 vs 4:[12, 11]
    4 vs 5:[12, 8]
    5 vs 6:[10, 9]
    6 vs 1:[13, 10]
    max_dice_val:6
    max_win:14

   * And 3 dice with 12 sides (75s):
    dice:
    [1, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6]
    [3, 3, 3, 4, 5, 6, 6, 6, 6, 6, 6, 6]
    [2, 2, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6]
    competitions:
    1 vs 2:[47, 44]
    2 vs 3:[52, 46]
    3 vs 1:[52, 50]
    max_dice_val:6
    max_win:52


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

# m: number of dice 
# n: number of sides of each die
function nontransitive_dice(m=3,n=6,max_dice_val=6,print_solutions=true,all_solutions=true,timeout=6)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>all_solutions, 
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

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

                                                            "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    # The dice
    # m: number dice 
    # n: sides per die
    println("number of dice: $m")
    println("number of sides per die: $n")
    @variable(model, 1 <= dice[1:m,1:n] <= max_dice_val, Int)

    # the competitions: 
    # (die 1 vs die 2, die 2 vs die 1), ... (die m vs die 1, die 1 vs die m)
    # And the first die must beat the second in each round.
    # Note the last wrap around which breaks the transitivity.
    @variable(model, 0 <= comp[1:m,1:2] <= n*n, Int)

    # Symmetry breaking
    @constraint(model, dice[1,1] == 1)

    # Symmetry breaking: order the number of each die (increasing)
    for dd in 1:m
        increasing(model, dice[dd,:])
    end

    # and now we roll...
    # Number of wins for [A vs B, B vs C, C vs A]
    b1 = @variable(model, [1:m,1:n,1:n], binary=true)
    b2 = @variable(model, [1:m,1:n,1:n], binary=true)
    for dd in 1:m
        # which dice to compare?
        d1 = dd
        d2 = 1+mod(dd,m)
        
        # This don't work (yet)!
        # @constraint(model, comp[dd,1] == sum([dice[d1, r1] > dice[d2, r2]
        #                for r1 in 1:n, r2 in 1:n]))

        # @constraint(model, comp[dd,2] == sum([dice[d2, r1] > dice[d1, r2]
        #                                for r1 in 1:n, r2 in 1:n]))

        # Rewrite:
        println("match $dd: d1:$d1 vs d2:$d2")
        for r1 in 1:n, r2 in 1:n
            # d1 > d2
            @constraint(model, b1[dd,r1,r2] := {dice[d1, r1] > dice[d2, r2]})
            # d2 > d1
            @constraint(model, b2[dd,r1,r2] := {dice[d2, r1] > dice[d1, r2]})
        end 
        @constraint(model, comp[dd,1] == sum(b1[dd,:,:]))
        @constraint(model, comp[dd,2] == sum(b2[dd,:,:]))
    end

    # non-transitivity
    # All dice 1..m-1 must beat the follower, and die m must beat die 1
    for dd in 1:m
        @constraint(model, comp[dd,1] > comp[dd,2])
    end
    @constraint(model, comp[m,1] > comp[1,2])

    # symmetry breaking
    # TODO! (lex_lesseq is defined in constraints_util.jl but either wrong or too slow!)
    # for dd in 2:m
    #    lex_less_eq(model,dice[dd-1,:],dice[dd,:])
    # end

    # Too slow!
    # @variable(model, 1 <= max_val <= max_dice_val, Int)
    # @constraint(model, max_val .>= dice)
    # @objective(model, Min, max_val)

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # max_val_val = convert.(Integer,JuMP.value.(max_val; result=sol))
                # println("max_val:$max_val_val", )
                dice_val = convert.(Integer,JuMP.value.(dice; result=sol))
                println("dice:")
                for r in eachrow(dice_val)
                    println(r)
                end
                println("competitions:")
                comp_val = convert.(Integer,JuMP.value.(comp; result=sol))
                for i in 1:m
                    println("$i vs $(1+mod(i,m)):", comp_val[i,:])
                end
                println("max_dice_val:", maximum(dice_val))
                println("max_win:", maximum(comp_val))
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end


@time nontransitive_dice(3,6,6,true,false,6)
# @time nontransitive_dice(3,6,4,true,false,6)
# @time nontransitive_dice(4,6,6,true,false,10)
# @time nontransitive_dice(5,6,6,true,false,20)
@time nontransitive_dice(6,6,6,true,false,20)
# @time nontransitive_dice(3,12,6,true,false,120) # 3 12 sided dice