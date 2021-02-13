#=

  Mr Greenguest puzzle (fancy dress) in ConstraintSolver.jl

  Problem (and LPL) code in
 
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
 
  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """

  The solution:
    t = 1
    h = 0
    r = 1
    s = 0
    n = 0
    cost = 10


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function fancy(print_solutions=true,all_solutions=true,timeout=6)

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

    @variable(model, x[1:5], Bin)
    T,H,R,S,N = x 
    @variable(model, 0 <= cost <= 10000, Int)
    
    #  This is a straight translation from the LPL 
    # 
    # ( ((T ==1) => (R ==1)) #\/ N ==1)  % Picat constraint
    @constraint(model, T := { R ==1 || N ==1 } )

    # ( ((S ==1 \/ R==1) => (T==1 \/ H==1)) \/ N==1)
    b1 = @variable(model, binary=true)
    @constraint(model, b1 := {S ==1 || R==1})
    b2 = @variable(model, binary=true)
    @constraint(model, b2 := {N==1})
    @constraint(model, b1 => {T ==1 || H ==1 } )
    @constraint(model, b1 + b2 >= 1)

    # ( ((R ==1 \/ H==1 \/ (S!=1)) => T==1) \/ N==1)
    b3 = @variable(model, binary=true)
    @constraint(model, b3 := {R ==1 || H==1 || S!=1})
    @constraint(model, b1 + b3 >= 1)
    
    @constraint(model, cost == 10*T + 2*H + 12*S + 11*N)
 

    @objective(model,Min,cost)

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
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                cost_val = convert.(Integer,JuMP.value.(cost; result=sol))                
                println("x:$x_val  cost:$cost_val")
                println("t: $(x_val[1])")
                println("h: $(x_val[2])")
                println("r: $(x_val[3])")
                println("s: $(x_val[4])")
                println("n: $(x_val[5])")
                println("cost: $cost_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time fancy()
