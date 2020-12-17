#=

  Secret Santa problem in Julia ConstraintSolver.jl

  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  """
  Honoring a long standing tradition started by my wife's dad, my friends
  all play a Secret Santa game around Christmas time. We draw names and
  spend a week sneaking that person gifts and clues to our identity. On the
  last night of the game, we get together, have dinner, share stories, and,
  most importantly, try to guess who our Secret Santa was. It's a crazily
  fun way to enjoy each other's company during the holidays.

  To choose Santas, we use to draw names out of a hat. This system was
  tedious, prone to many "Wait, I got myself..." problems. This year, we
  made a change to the rules that further complicated picking and we knew
  the hat draw would not stand up to the challenge. Naturally, to solve
  this problem, I scripted the process. Since that turned out to be more
  interesting than I had expected, I decided to share.

  This weeks Ruby Quiz is to implement a Secret Santa selection script.

  Your script will be fed a list of names on STDIN.
  ...
  Your script should then choose a Secret Santa for every name in the list.
  Obviously, a person cannot be their own Secret Santa. In addition, my friends
  no longer allow people in the same family to be Santas for each other and your
  script should take this into account.
  """

  Comment: Well, this model skips the file input and mail parts. We
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families.

           And we also do some random family configurations below.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using Distributions
const CS = ConstraintSolver
include("constraints_utils.jl")

function secret_santa(family,print_solutions=true,all_solutions=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
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

    n = length(family)
    @variable(model, 1 <= x[1:n] <= n, Int)

    # Everyone gives and receives a Secret Santa
    @constraint(model, x in CS.AllDifferentSet())

    # Can't be one own's Secret Santa
    for i in 1:n
        @constraint(model, x[i] != i)
    end

    # No Secret Santa to a person in the same family
    @variable(model,1 <= fxi[1:n] <= n, Int)
    for i in 1:n
        my_element(model, x[i],family,fxi[i])
        @constraint(model,family[i] != fxi[i])
    end

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        if all_solutions
            println("num_sols:$num_sols\n")
        end

        if print_solutions
            for sol in 1:num_sols
                # println("solution #$sol")
                xx = convert.(Integer,JuMP.value.(x; result=sol))
                println("x:$xx")

            end
        end
        if all_solutions
            println("num_sols:$num_sols\n")
        end
    else
        println("status:$status")
    end

    return status
end

#
# Testing
#

# The families
secret_santa_problems =  Dict(
:1 =>  [1,1,2,2, 3, 4,4], # Ruby Quiz example. 672 solutions
:2 =>  [1,1,1,1, 2, 3,3,3,3,3, 4,4], # From my Picat model
)
print_solutions=false
all_solutions=true
# Number of solutions for the simple instance
@time secret_santa(secret_santa_problems[:1],print_solutions,all_solutions)

# Generate random families

function random_families(n=10)
    d = round(Int,n/2)
    rand(DiscreteUniform(1,d),n)|>sort
end

print_solutions=true
all_solutions=false

# Test larger instances
for n in [i*10 for i in 1:10]
    println("\nn:$(n)")
    family = random_families(n)
    println("family:$family")
    c = 0
    timeout = false
    while true && c < 10
        @time status = secret_santa(family,print_solutions,all_solutions)
        c += 1
        if status == MOI.TIME_LIMIT
            timeout = true
        end
        if status == MOI.OPTIMAL || timeout || c >= 10
            break
        end
    end
    if timeout
        break
    end
end
