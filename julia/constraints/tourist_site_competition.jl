#=

  Tourist Site Competition in ConstraintSolver.jl 

  From Pierre Flener's presentation 
  "Constraint Technology - A Programming Paradigm on the Rise"
  http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
     pages 5f: problem statement 
     pages 12f: model
     pages 21ff: walktrough of a solution

  With 7 tourist sites and 7 judges:
  """
  Every tourist site is visited by r = 3 judges.
  Every judge visits c = 3 tourist sites.
  Every pair of sites is visited by lambda = 1 common judge.
  """

  There are 151200 solutions to this problem.
  With the additional constraint that Ali should visit Birka, Falun and Lund
  there are 4320 solutions.


  This problem was also presented as "The Airline-of-the-Year Problem"
  in his (Flener's) presentation
  "Constraint Programming - Programming Paradigm on the Rise"
  http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
  page 4f
  The problem is stated as follows for 7 airlines and 7 judges:
  """
  Constant jury: Every airline is tested by 3 judges.
  Constant load: Every judge tests 3 airlines.
  Equity: Every airline pair is tested by 1 common judge.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function tourist_site_competition(instance, print_solutions=true,all_solutions=true,timeout=6)

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
                                                            "branch_split"=>:Biggest,
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

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    r = instance[:r]
    c = instance[:c]
    lambda = instance[:lambda]
    num_sites = instance[:num_sites]
    num_judges = instance[:num_judges]
    symmetry_breaking = instance[:symmetry_breaking]

    @variable(model, 0 <= x[1:num_sites,1:num_judges] <= 1, Int)

    # Every tourist site is visited by R judges.
    for row in eachrow(x)
        @constraint(model, sum(r) == r)
    end
 
    # Every judge visits C tourist sites.
    for col in eachcol(x) 
        @constraint(model, sum(col) <= c)
    end
 
    # Every pair of sites is visited by Lambda common judges.
    for s1 in 1:num_sites, s2 in 1:num_sites 
        if s1 < s2
            b = @variable(model, [1:num_judges], Bin )
            for j in 1:num_judges 
                @constraint(model, b[j] := {x[s1,j] == 1 && x[s1,j] == x[s2,j] } )
            end
            @constraint(model,lambda == sum(b))
        end 
    end

    # Symmetry breaking: 
    # Assign Judge 1 to the first R sites
    if symmetry_breaking
        for i in 1:r 
            @constraint(model, x[r,1] == 1)
        end
    end



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
                for row in eachrow(x_val)
                    println(row)
                end
                println("Assignments per sites ")
                for (site,judges) in enumerate(eachrow(x_val))
                    println("site $site: judges: ", findall(judges .== 1))
                end
                println("\nAssignments per judge ")
                for (judge,sites) in enumerate(eachcol(x_val))
                    println("judge $judge: sites: ", findall(sites .== 1))
                end

            end
        end
        println("num_sols:$num_sols\n")
    else
        println("status:$status")
    end

    return status, num_sols
end


instance = Dict(
   :r => 3,
   :c => 3,
   :lambda => 1,

   :num_sites => 7,
   :num_judges => 7,
   :symmetry_breaking => true, 

)

@time tourist_site_competition(instance,true,false,6)
