#=

  Global constraint among in ConstraintSolver.jl

  Among: Requires exactly 'n' variables in 'x' to take one of the values in 'v'.

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong.html
  """
  Constraint
    among(NVAR,VARIABLES,VALUES)
  ...
  Purpose
    NVAR is the number of variables of the collection VARIABLES that
    take their value in VALUES.

  Example:
  (3, <4, 5, 5, 4, 1>, <1,5,8>)

  The among constraint holds since exactly 3 values of the collection
  of values 
  <4, 5, 5, 4, 1> belong to the set of values {1, 5, 8}.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")



function among_test(len=5,d=8,nnn=nothing,xxx=nothing,vvv=nothing,print_solutions=true,all_solutions=true,timeout=6)

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
    println("len=$len,d=$d,nnn=$nnn,xxx=$xxx,vvv=$vvv,print_solutions=$print_solutions,all_solutions=$all_solutions,timeout=$timeout")
    @variable(model, 1 <= x[1:len] <= d, Int)
    n = @variable(model, integer=true, lower_bound=0,upper_bound=len)

    @variable(model, 1 <= v[1:3] <= d, Int)

    if nnn !== nothing 
        @constraint(model, n .== nnn)
    end

    if vvv !== nothing 
        @constraint(model, v .== vvv )
    end 
    increasing_strict(model, v)

    among(model,n,x,v)

    # Show all x for which n=3
    @constraint(model, n==3)

    if xxx !== nothing
        @constraint(model, x .== xxx)
    end

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    num_sols = 0
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        # println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                v_val = convert.(Integer,JuMP.value.(v; result=sol))
                n_val = convert.(Integer,JuMP.value.(n; result=sol))
                println("x:$x_val   n:$n_val  v:$v_val")

            end
        end
        println("num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status, num_sols
end

#
# Some test cases
#
tests = Dict(
    :0 => Dict(
        :len => 5,
        :d   => 8,
        :nnn => 3,
        :xxx => nothing,
        :vvv => [1,3,8],
        :print_solutions => true,
        :all_solutions => false,
    ), 
   :1 => Dict(
       :len => 5,
       :d   => 8,
       :nnn => 3,
       :xxx => nothing,
       :vvv => [1,3,8],
       :print_solutions => false,
       :all_solutions => true,
   ),
   :2 => Dict(
       :len => 5,
       :d   => 8,
       :nnn => 3,
       :xxx => [3,1,1,1,2],
       :vvv => nothing,
       :print_solutions => true,
       :all_solutions => true,
   ),
)

for p in sort(collect(keys(tests)))
    println("p:$p")
    t = tests[p]
    among_test(t[:len],t[:d],t[:nnn],t[:xxx],t[:vvv],t[:print_solutions],t[:all_solutions])
end 

