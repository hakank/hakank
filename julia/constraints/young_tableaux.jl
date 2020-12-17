#=
  Young tableaux and partition in Julia ConstraintSolver.jl

  See
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

  1.   1 2 3 4

  2.   1 2 3         1 2 4    1 3 4
       4             3        2

  3.   1 2           1 3
       3 4           2 4

  4    1 2           1 3      1 4
       3             2        2
       4             4        3

  5.   1
       2
       3
       4
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# Note: count_ctr is defined in constraints_utils.jl
#
function young_tableaux(n=5)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> true,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))

    println("Young tableaux and partitions of order $n")

    # the Young tableuax grid
    @variable(model, 1 <= x[1:n,1:n] <= n+1, Int)

    # the partition structure p
    @variable(model, 0 <= p[1:n] <= n, Int)

    # 1..N is used exactly once (N+1 may be used many times)
    for i in 1:n
        # This is how I would like to model it:
        # @constraint(model,(1 == sum(x[:]==i)))
        # @constraint(model, 1 == sum([x[j,k] == i for j in 1:n, k in 1:n] ))
        # @constraint(model, 1 == sum([xflat[j] == i for j in 1:length(xflat)] ))
        count_ctr(model, x,:(==), i, 1)

    end

    # First position
    @constraint(model, x[1,1] == 1)

    # All rows and columns should be ordered
    for i in 1:n
        increasing(model,x[i,:]) # rows
        increasing(model,x[:,i]) # columns
    end


    # Calculate the structure (the partition)
    for i in 1:n
        # This is what I would like to write
        # @constraint(model, p[i] == sum([x[i,j] for j in 1:n]))
        # This works:
        count_ctr(model,x[i,:],:(<=),n, p[i])

    end

    # P should be ordered
    decreasing(model,p)

    # We just work with n numbers
    @constraint(model, n == sum(p))


    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    if status == MOI.OPTIMAL

        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")

        for sol in 1:num_sols
            println("\nsolution #$sol")
            xx = convert.(Integer,JuMP.value.(x,result=sol))
            pp = convert.(Integer,JuMP.value.(p,result=sol))
            for i in 1:n
                for j in 1:n
                    if xx[i,j] <= n
                        print(xx[i,j]," ")
                    else
                        print("_ ")
                    end
                end
                println()
            end
            println("p:$pp")

        end
    end
end

young_tableaux(5)
