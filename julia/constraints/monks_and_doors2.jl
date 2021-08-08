#=

  Monks and doors problem in ConstraintSolver.jl

  From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
  """
  There is a room with four doors and eight monks. One or more of
  the doors may be exit. Each monk is either telling a lie or the truth.
 
  The monks make the following statements:
  Monk 1: Door A is the exit.
  Monk 2: At least one of the doors B and C is the exit.
  Monk 3: Monk 1 and Monk 2 are telling the truth.
  Monk 4: Doors A and B are both exits.
  Monk 5: Doors A and B are both exits.
  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
 
  Which door is an exit no matter who is a liar and who is telling the
  truth.
  """
 
  Answer: Door A is an exit.
          And monks 1, 7, and 8 are telling the truth.

  Basic Boolean algebra, needed since ConstratinSolver.jl don't (yet) 
  support more advance constraints in a reification:
     A /\ B  :  A + B == 2 
     A \/ B  :  A + B >= 1 
     A xor B :  A + B == 1
     A => B  :  A <= B
     A <=> B :  A == B

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function monks_and_doors(print_solutions=true,all_solutions=true,timeout=6)

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

    num_doors = 4
    num_monks = 8
    @variable(model, doors[1:num_doors], Bin)
    da,db,dc,dd = doors 
    door_names = ["A","B","C","D"]

    @variable(model, monks[1:num_monks], Bin)
    m1,m2,m3,m4,m5,m6,m7,m8 = monks

    # Monk 1: Door A is the exit.
    # M1 #= A (Picat constraint)
    @constraint(model, m1 == da)
    
    #  Monk 2: At least one of the doors B and C is the exit.
    # M2 #= 1 #<=> (B #\/ C)
    # @constraint(model, m2 := { db + dc >= 1})
    # @constraint(model, m2 := { db == 1 || dc == 1})
    @constraint(model, m2 := { db || dc})
    
    #  Monk 3: Monk 1 and Monk 2 are telling the truth.
    # M3 #= 1 #<=> (M1 #/\ M2)
    # @constraint(model, m3 := { m1 + m2 == 2})
    # @constraint(model, m3 := { m1 == 1 && m2 == 1})
    @constraint(model, m3 := { m1 && m2})
    
    #  Monk 4: Doors A and B are both exits.
    # M4 #= 1 #<=> (A #/\ B)
    # @constraint(model, m4 := { da + db == 2})
    # @constraint(model, m4 := { da == 1 && db == 1})
    @constraint(model, m4 := { da && db })
    
    #  Monk 5: Doors A and C are both exits.
    # M5 #= 1 #<=> (A #/\ C)
    # @constraint(model, m5 := { da + dc == 2})
    # @constraint(model, m5 := { da == 1 && dc == 1})
    @constraint(model, m5 := { da && dc })
    
    #  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
    # M6 #= 1 #<=> (M4 #\/ M5)
    # @constraint(model, m6 := { m4 + m5 == 1})
    # @constraint(model, m6 := { m4 == 1|| m5 == 1})
    @constraint(model, m6 := { m4 || m5 })

    
    #  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
    # M7 #= 1 #<=> (M3 #=> M6)
    # @constraint(model, m7 := { m3 <= m6}) # ORIG
    # @constraint(model, m7 := { m3 => m6})  # This don't work!
    @constraint(model, m7 := { m3 => {m6 == 1 }}) 

    #  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
    # M8 #= 1 #<=> ((M7 #/\ M8) #=> (M1))
    b1 = @variable(model, binary=true)
    @constraint(model, b1 := {m7 == 1 && m8 == 1})
    @constraint(model, m8 := {b1 <= m1})

    # TEST: 
    # LoadError: TypeError: non-boolean (VariableRef) used in boolean context
    # @constraint(model, m8 := { (m7 && m8) => { m1 == 1 } }) # TEST This don't work!


    # TEST #feature-indicator-in-reified
    # @constraint(model, m8 := {(m7 == 1 && m8 == 1) => m1 == 1}) # don't work 
    # @constraint(model, m8 := {(m7 == 1 && m8 == 1) => {m1 == 1}}) # don't work
    
    # Exactly one door is an exit.
    # (A + B + C + D) #= 1
    @constraint(model, da + db + dc + dd == 1)
    

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
                #println("solution #$sol")
                doors_val = convert.(Integer,JuMP.value.(doors; result=sol))
                monks_val = convert.(Integer,JuMP.value.(monks; result=sol))
                println("doors:$doors_val  monks:$monks_val")

                println("the exit door          : ", [door_names[i] for i in 1:num_doors if doors_val[i] == 1 ][1] )
                println("monks telling the truth: ", [m for m in 1:num_monks if monks_val[m] == 1 ] )

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time monks_and_doors()
