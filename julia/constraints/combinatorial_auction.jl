#=

  Combinatorial auction in ConstraintSolver.jl 

  http://en.wikipedia.org/wiki/Combinatorial_auction
  """
  A combinatorial auction is an auction in which bidders can place 
  bids on combinations of items, or "packages," rather than 
  just individual items. Simple combinatorial auctions have been 
  used for many years in estate auctions, where a common procedure 
  is to auction the individual items and then at the end to accept 
  bids for packages of items.
  """

  This simple example is from the lecture slides
  Constraint Satisfaction Problems, Constraint Optimization
  by Bernhard Nebel and Stefan Wölfl
  http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
  """
  In combinatorial auctions, bidders can give bids for set of items.
  The auctioneer [then] has to generate an optimial selection, e.g.
  one that maximizes revenue.
  
  Definition
  The combinatorial auction problem  is specified as follows:
    Given: A set of items Q = {q1,...,qn} and a set of bids
           B = {b1,...,bm} such that each bid is bi = (Qi, ri),
           where Qi (= Q and ri is a strictly positive real number.
    Task: Find a subset of bids B'(= B such that any two bids in B'
          do not share an item maximizing Sum(Qi,ri) (= Biri.

  ...

  Example Auction

  Consider the following auction:
    b1 = {1,2,3,4}, r1 = 8
    b2 = {2,3,6},   r2 = 6
    b3 = {1,4,5},   r3 = 5
    b4 = {2,8},     r4 = 2
    b5 = {5,6},     r5 = 2

  What is the optimal assignment?
  """ 


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Picat page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function combinatorial_auction(problem, print_solutions=true,all_solutions=true,timeout=6)

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
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    num_items = problem[:num_items]
    num_bids  = problem[:num_bids]
    packages  = problem[:packages]
    bids      = problem[:bids]

    @variable(model, x[1:num_bids], Bin)
    @variable(model, 0 <= total <= sum(bids), Int)

    @constraint(model, total == sum(x.*bids))

    # ensure that each item is selected atmost once
    for j in 1:num_items
        @constraint(model, sum([x[i] for i in 1:num_bids if j in packages[i]]) <= 1)
    end

    @objective(model,Max,total)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                total_val = convert.(Integer,JuMP.value.(total; result=sol))
                println("x:$x_val")
                println("total:$total_val")
                for b in 1:num_bids 
                    if x_val[b] == 1 
                        println("bid:$b items:$(packages[b])")
                    end
                end
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

#
# Problem instances
#
problems = Dict(
    #
    # The example cited above
    #
    :1 => Dict(
        :num_items => 7,
        :num_bids => 5,
        :packages => [[1,2,3,4],
                      [2,3,6],
                      [1,4,5],
                      [2,7],
                      [5,6]],
        :bids => [8,6,5,2,2]
    ),

    #
    # From Numberjack Tutorial, page 24 (slide 51/175)
    #
    :2 => Dict( 
            :num_items => 4,
            :num_bids => 5,
            :packages => [[1,2],
                          [1,3],
                          [2,4],
                          [2,3,4],
                          [1]],
            :bids => [8,6,5,2,2]
        ),

    #
    # From "A Faster Core Constraint Generation Algorithm for Combinatorial Auctions"
    # Benedikt Bunz, Sven Seuken, Benjamin Lubin
    # page 4
    #
    :3  => Dict(
        :num_items => 5,
        :num_bids => 7,
        :packages => [[1],
                      [2],
                      [3],
                      [4],
                      [1,2,3,4,5],
                      [1,2,5],
                      [3,4,5]],
        :bids => [10,10,10,10,12,8,8]
    ),


    #
    # From "Combinatorial Auctions:  Complexity and Algorithms"
    # Martin Bichler
    # page 4 (table 1)
    #
    # Line  Bids                       B1    B2   B3   B4
    #    1  1000t grain in Berlin      1     0    1    1
    #    2  800t grain in Munich       0     1    1    1
    #    3  800t grain in Vienna       1     1    1    0
    #    4  Bid price (in thousands) 150   125   300 125
    #
    :4 => Dict(
            :num_items => 3,
            :num_bids => 4,
            :packages => [[1,3],
                          [2,3],
                          [1,2,3],
                          [1,2]],
            :bids => [150,125,300,125],
    ),

)


# @time combinatorial_auction(problems[:1])
for p in sort(collect(keys(problems)))
    println("\nproblem:$p")
    @time combinatorial_auction(problems[p])
end
