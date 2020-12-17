#=

  Seseman problem in Julia + ConstraintSolver.jl

  Description of the problem:

  n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.

  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:

   a b c
   d   e
   f g h

  Where the following constraints must hold:

    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum


  For a (Swedish) discussion of this problem, see
  "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using ECLiPSe CLP code)

  It was also is commented in the (Swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/


=#
using ConstraintSolver, JuMP
using Cbc, GLPK
const CS = ConstraintSolver

include("constraints_utils.jl")

#
# first_num = 1:
# * without symmetry breaking: 85 solutions
# * with symmetry breaking: 4 solutions
#
# first_num = 0:
# * without symmetry breaking: 231 solutions
# * with symmetry breaking: 7 solutions#
#
function seseman(row_sum=9,total=24,first_num=1;all_solutions=false,print_solutions=true,symmetry_breaking=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            "simplify"=>true,

                                                            "time_limit"=>14,
                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                        ))

    println("row_sum:$row_sum total:$total first_num:$first_num symmetry_breaking:$symmetry_breaking")


    @variable(model, first_num <= x[1:8] <= 9, Int)
    a,b,c,d,e,f,g,h = x

    @constraint(model, sum(x) == total)
    
    segments = [[a,b,c],
                [a,d,f],
                [c,e,h],
                [f,g,h]]
    for segment in segments
        @constraint(model, sum(segment) == row_sum)
    end

    if symmetry_breaking
        syms = [[a,h],[b,d],[d,e],[e,g]]
        for s in syms
            @constraint(model, s[1] <= s[2])
        end
    end

    # Solve the problem
    # println("solve")
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        # num_sols2 = JuMP.result_count(model) # same result
        # println("num_sols2:$num_sols2\n")

        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                xx = convert.(Integer,JuMP.value.(x,result=sol))
                println("$(xx[1])  $(xx[2])  $(xx[3])")
                println("$(xx[4])     $(xx[5])")
                println("$(xx[6])  $(xx[7])  $(xx[8])")
            end
        end
    end
end

# First num: 1
seseman(9,24,1;all_solutions=true,print_solutions=true,symmetry_breaking=true)

# First num: 0
# seseman(9,24,0;all_solutions=true,symmetry_breaking=false)

println("\nChecking different configurations")
for first_num in [0,1], symmetry_breaking in [true,false]
    seseman(9,24,first_num;all_solutions=true,print_solutions=false,symmetry_breaking=symmetry_breaking)
end
