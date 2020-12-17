#=
  Coins puzzle in Julia + ConstraintSolver.jl.

  Problem from
  Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one
  should place coins in such a way that the following conditions are
  fulfilled:
    1. In each row exactly 14 coins must be placed.
    2. In each column exactly 14 coins must be placed.
    3. The sum of the quadratic horizontal distance from the main
       diagonal of all cells containing a coin must be as small as possible.
    4. In each cell at most one coin can be placed.

   The description says to place 14x31 = 434 coins on the chessboard
   each row containing 14 coins and each column also containing 14 coins.
  """

  Note: This problem is quite hard for CP solvers. A MIP solver solves
  the (14,31) problem in millis.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc
using GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

function coins_grid(n=31,c=14)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer) # , "logLevel" => 0)
    model = Model(optimizer_with_attributes(CS.Optimizer,   # "all_solutions"=> true,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            "simplify"=>true,

                                                            "time_limit"=>110,
                                                            # "lp_optimizer" => cbc_optimizer, # 10.7s
                                                            "lp_optimizer" => glpk_optimizer, # 1.1s
                                        ))

    @variable(model, x[1:n,1:n], Bin)
    @variable(model, 0 <= s <= n*n*n, Int)

    @constraint(model,s == sum([(x[i,j] * abs(i-j)*abs(i-j)) for i in 1:n, j in 1:n]))
    for i in 1:n
        @constraint(model, c == sum(x[i,:]))
        @constraint(model, c == sum(x[:,i]))
    end

    @objective(model, Min, s)

    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL
        x = convert.(Integer,JuMP.value.(x))
        s = convert.(Integer,JuMP.value.(s))
        for i in 1:n
            println(x[i,:])
        end
        println("s:$s")

        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")
    end
end

n = 31
c = 14
coins_grid(n,c)
