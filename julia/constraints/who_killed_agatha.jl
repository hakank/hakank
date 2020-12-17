#=
  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in Julia + ConstraintSolver.jl.

  This is a standard benchmark for theorem proving.

  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
  """
  Someone in Dreadsbury Mansion killed Aunt Agatha.
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and
  are the only ones to live there. A killer always hates, and is no
  richer than his victim. Charles hates noone that Agatha hates. Agatha
  hates everybody except the butler. The butler hates everyone not richer
  than Aunt Agatha. The butler hates everyone whom Agatha hates.
  Noone hates everyone. Who killed Agatha?
  """

  Originally from F. J. Pelletier:
  Seventy-five problems for testing automatic theorem provers.
  Journal of Automated Reasoning, 2: 191 216, 1986.
  http://www.sfu.ca/~jeffpell/papers/75ATPproblems86.pdf


  I have blogged about the problem here:
  * "Learning constraint programming - part II: Modeling with the Element constraint"
    http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin.html
  * "Learning Constraint Programming IV: Logical constraints: Who killed Agatha? revisited"
  http://www.hakank.org/constraint_programming_blog/2009/05/learning_constraint_programmin_3.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")

function print_matrix(x)
    print_grid(x)
end

function who_killed_agatha()

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

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer, # 45.8s
                                                            # "lp_optimizer" => glpk_optimizer, # 21.5s
                                        ))

    n = 3
    agatha = 1
    butler = 2
    charles = 3
    @variable(model, 1 <= killer <= 3, Int)
    @variable(model, hates[1:n,1:n], Bin)
    @variable(model, richer[1:n,1:n], Bin)
    @variable(model, is_killer[1:3], Bin)

    # Connect killer to is_killer
    for i in 1:n
        @constraint(model, is_killer[i] := {killer == i})
    end
    @constraint(model, sum(is_killer) == 1)

    #
    # Constraints
    #
    # Agatha, the butler, and Charles live in Dreadsbury Mansion, and
    # are the only ones to live there.
    #

    # * A killer always hates, and is no richer than his victim.
    for i in 1:n
        @constraint(model, is_killer[i] => {hates[i, agatha] == 1})
        @constraint(model, is_killer[i] => {richer[i, agatha] == 0})
    end

    # * Define the concept of richer: no one is richer than him-/herself
    for i in 1:n
        @constraint(model,richer[i,i]== 0)
    end

    # (contd...) if i is richer than j then j is not richer than i
    for i in 1:n, j in 1:n
        if i != j
            @constraint(model, richer[i,j] => {richer[j,i] == 0})
            @constraint(model, !richer[j,i] => {richer[i,j] == 1})
        end
    end

    # * Charles hates no one that Agatha hates.
    for i in 1:n
        @constraint(model, hates[agatha, i] => {hates[charles, i] == 0})
    end

    # * Agatha hates everybody except the butler.
    @constraint(model,hates[agatha, butler] == 0)
    @constraint(model,hates[agatha, charles] == 1)
    @constraint(model,hates[agatha, agatha] == 1)

    # * The butler hates everyone not richer than Aunt Agatha.
    for i in 1:n
      @constraint(model, !richer[i, agatha] => {hates[butler, i] == 1})
    end

    # * The butler hates everyone whom Agatha hates.
    for i in 1:n
        @constraint(model, hates[agatha, i] => {hates[butler, i] == 1})
    end

    # * No one hates every one.
    for i in 1:n
        @constraint(model, sum([hates[i,j] for j in 1:n]) <= 2)
    end

    # * A killer always hates, and is no richer than his victim.
    for i in 1:n
       @constraint(model, is_killer[i] => {hates[i,agatha] == 1})
       @constraint(model, is_killer[i] => {richer[i,agatha] == 0})
    end

    # * Who killed Agatha?


    # Solve the problem
    println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    println("status:$status")
    solutions = []
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("\nnum_sols:$num_sols\n")

        for sol in 1:num_sols
            # println("solution #$sol")
            xkiller = convert.(Integer,JuMP.value.(killer,result=sol))
            # println("killer: $xkiller\n")
            push!(solutions,xkiller)
        end
    end
    people = ["agatha","butler","charles"]
    println("solutions: $([people[sol] for sol in solutions])")
end

who_killed_agatha()
