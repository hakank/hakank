#=#
  Langford's number problem L(2,N) in Julia ConstraintSolver.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i,
  each subsequent occurrence of i, appears i+1 indices later
  than the last.
  For example, for k=4, a solution would be 41312432
  """

  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html

  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  Note: For k=4 there are two different solutions:
     solution:[4,1,3,1,2,4,3,2]
     position:[2,5,3,1,4,8,7,6]
  and
     solution:[2,3,4,2,1,3,1,4]
     position:[5,1,2,3,7,4,6,8]

  Which this symmetry breaking

     Solution[1] #< Solution[K2],

  then just the second solution is shown.

  Note: There are only solutions when K mod 4 == 0 or K mod 4 == 3.

  - For a generalized version L(k,n) where k >= 2, see http://hakank.org/langford_generalized.pi
  - For Nickerson's variant, see http://hakank.org/langford_nickerson.pi

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/


=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver
include("constraints_utils.jl")


#
# Note: my_element is defined in constraints_utils.jl
#
function langford(k=5,symmetry_breaking=true,all_solutions=true,print_solutions=true)

    if !((k % 4 == 0) || (k % 4 == 3))
        println("There is no solutions for k ($k) unless K mod 4 == 0 or K mod 4 == 3")
        return
    end

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS, # <-
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf, # <-

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>16,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))
    k2 = 2*k

    @variable(model, 1 <= position[1:k2] <= k2, Int)
    @variable(model, 1 <= solution[1:k2] <= k, Int)

    @constraint(model, position in CS.AllDifferentSet())

    # symmetry breaking
    if symmetry_breaking
        @constraint(model, solution[1] <= solution[k2])
    end

    for i in 1:k
        @constraint(model, position[k+i] == position[i] + i+1)
        # @constraint(model, solution[position[i]] == i )
        my_element(model, position[i], solution, i)
        my_element(model, position[k+i], solution, i)
    end


    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                positionx = convert.(Integer,JuMP.value.(position; result=sol))
                solutionx = convert.(Integer,JuMP.value.(solution; result=sol))
                # println("position:$positionx\n")
                println("solution:$solutionx position:$positionx\n")
            end
        end
    else
        println("status:$status")
    end

    return status
end

all_solutions = true
print_solutions = true
symmetry_breaking = true
@time langford(7,symmetry_breaking,all_solutions,print_solutions)

for n in 1:20
    if n % 4 == 3  || n % 4 == 0
        println("n:$n")
        @time status = langford(n,true,all_solutions,false)
        if status != MOI.OPTIMAL
            break
        end
    end
end

println("\nTime to first solution:")
all_solutions = false
print_solutions = true
symmetry_breaking = true
for n in 1:20
    if n % 4 == 3  || n % 4 == 0
        println("n:$n")
        @time status = langford(n,symmetry_breaking,all_solutions,print_solutions)
        if status != MOI.OPTIMAL
            break
        end
    end
end
