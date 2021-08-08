#=

  Secret Santa problem II in Julia ConstraintSolver.jl

  From Maple Primes: "Secret Santa Graph Theory"
  http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
  """
  Every year my extended family does a "secret santa" gift exchange.
  Each person draws another person at random and then gets a gift for
  them. At first, none of my siblings were married, and so the draw was
  completely random. Then, as people got married, we added the restriction
  that spouses should not draw each others names. This restriction meant
  that we moved from using slips of paper on a hat to using a simple
  computer program to choose names. Then people began to complain when
  they would get the same person two years in a row, so the program was
  modified to keep some history and avoid giving anyone a name in their
  recent history. This year, not everyone was participating, and so after
  removing names, and limiting the number of exclusions to four per person,
  I had data something like this:

  Name: Spouse, Recent Picks

  Noah: Ava. Ella, Evan, Ryan, John
  Ava: Noah, Evan, Mia, John, Ryan
  Ryan: Mia, Ella, Ava, Lily, Evan
  Mia: Ryan, Ava, Ella, Lily, Evan
  Ella: John, Lily, Evan, Mia, Ava
  John: Ella, Noah, Lily, Ryan, Ava
  Lily: Evan, John, Mia, Ava, Ella
  Evan: Lily, Mia, John, Ryan, Noah
  """

  Note: I interpret this as the following three constraints:
    1) One cannot be a Secret Santa of one's spouse
    2) One cannot be a Secret Santa for somebody two years in a row
    3) Optimization: maximize the time since the last time

  This model also handle single persons, something the original
  problem don't mention.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function secret_santa2(n,M,people,spouses,rounds,print_solutions=true,all_solutions=true)

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

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    @variable(model, 1 <= santas[1:n] <= n, Int)
    @variable(model, 1 <= santas2[1:n] <= n, Int)

    @variable(model, 1 <= santa_distance[1:n] <= n+1, Int)
    @variable(model, 0 <= z <= 1000, Int)

    # Everyone gives and receives a Secret Santa
    @constraint(model, santas in CS.AllDifferent())

    # no Santa for a spouses (or him-/herself)
    for i in 1:n
        @constraint(model,santas[i] != i)
        if spouses[i] > 0
            @constraint(model,santas[i] != spouses[i])
        end
    end

    # optimize "distance" to earlier rounds:
    for i in 1:n
        # SantaDistance[I] == Rounds[I,Santas[I]]
        my_element(model,santas[i],rounds[i,:],santa_distance[i])
    end

    # Cannot be a Secret Santa for the same person two years in a row.

    for i in 1:n
       # Rounds[I,Santas2[I]] == 1,
       my_element(model,santas2[i],rounds[i,:],1)
       @constraint(model,santas[i] != santas2[i])
    end

    @constraint(model,z == sum(santa_distance))

    # Optimize the sum of santa_distance
    @objective(model,Max,z)


    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                z_val = convert.(Integer,JuMP.value.(z; result=sol))
                santas_val = convert.(Integer,JuMP.value.(santas; result=sol))
                santas2_val = convert.(Integer,JuMP.value.(santas2; result=sol))
                santa_distance_val = convert.(Integer,JuMP.value.(santa_distance; result=sol))
                println("z:$z_val")
                println("santas:$santas_val")
                println("santas2:$santas2_val")
                println("santa_distance:$santa_distance_val")
                println()

            end
        end
        println("num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status
end

#
# Test for n=8 (without single) or n= 9 with a single person
#
function test_secret_santa2(n=9,print_solutions=true,all_solutions=false)
    println("n:$n")
    # n = 8 # Without Single person
    # n = 9 # With a Single person
    noah,ava,ryan,mia,ella,john,lily,evan,single = 1:9
    people = ["noah","ava","ryan","mia","ella","john","lily","evan","single"]

    spouses = [
             ava,  # noa
             noah, # ava
             mia,  # rya
             ryan, # mia
             john, # ella
             ella, # john
             evan, # lily
             lily  # evan
             , 0   # single has no spouse
       ]

     M = n+1 # "large M" to indicate no earlier history

    #
    # The matrix version of earlier rounds.
    # M means that no earlier Santa.
    # Note: Ryan and Mia has the same recipient for years 3 and 4,
    #       and Ella and John has for year 4.
    #       This seems to be caused by modification of
    #       original data.
    #
    # rounds with a single person (fake data)
    #
    rounds = [
    #N  A  R  M  El J  L  Ev S
    [0, M, 3, M, 1, 4, M, 2, 2], # Noah
    [M, 0, 4, 2, M, 3, M, 1, 1], # Ava
    [M, 2, 0, M, 1, M, 3, 4, 4], # Ryan
    [M, 1, M, 0, 2, M, 3, 4, 3], # Mia
    [M, 4, M, 3, 0, M, 1, 2, M], # Ella
    [1, 4, 3, M, M, 0, 2, M, M], # John
    [M, 3, M, 2, 4, 1, 0, M, M], # Lily
    [4, M, 3, 1, M, 2, M, 0, M], # Evan
    [1, 2, 3, 4, M, 2, M, M, 0]  # Single
    ]

    secret_santa2(n,M,people,spouses,resize_matrix(rounds),print_solutions,all_solutions)

end

# First we solve for first optimal solutions
#=
z:67
santas:[4, 7, 1, 6, 2, 8, 3, 5]
santas2:[5, 8, 5, 2, 7, 1, 6, 4]
santa_distance:[9, 9, 9, 9, 4, 9, 9, 9]
  1.516136 seconds (24.50 M allocations: 552.591 MiB, 10.32% gc time)

z:90
santas:[7, 5, 6, 1, 9, 4, 3, 2, 8]
santas2:[5, 9, 5, 2, 7, 1, 6, 4, 1]
santa_distance:[10, 10, 10, 10, 10, 10, 10, 10, 10]
  1.831563 seconds (29.13 M allocations: 652.884 MiB, 11.81% gc time)

3.794155 seconds (54.49 M allocations: 1.221 GiB, 10.56% gc time)
=#
@time test_secret_santa2(8,true,false)
@time test_secret_santa2(9,true,false)

# Then the total number of solutions
#=
n:8
num_sols:8
  2.428113 seconds (39.62 M allocations: 904.140 MiB, 9.26% gc time)

n:9
num_sols:24
  3.991528 seconds (58.10 M allocations: 1.314 GiB, 17.37% gc time)

10.229509 seconds (152.62 M allocations: 3.439 GiB, 12.86% gc time)
=#
@time test_secret_santa2(8,false,true)
@time test_secret_santa2(9,false,true)
