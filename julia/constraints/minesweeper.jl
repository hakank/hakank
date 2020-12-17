#=
  Minesweeper solver in Julia + ConstraintSolver.jl

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric
  characters represent the number of mines adjacent to that field.
  Dots represent fields with an unknown number of mines adjacent to
  it (or an actual mine).
  """

  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """

  Also see:
  * http://www.janko.at/Raetsel/Minesweeper/index.htm

  * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

  * Ian Stewart on Minesweeper:
    http://www.claymath.org/Popular_Lectures/Minesweeper/

  * Richard Kaye's Minesweeper Pages
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm

  * Some Minesweeper Configurations
    http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/
=#

using ConstraintSolver, JuMP
const CS = ConstraintSolver
include("constraints_utils.jl")
include("minesweeper_problems.jl")

function minesweeper(problem,all_solutions=true,print_solutions=true, timeout=Inf)

    m = Model(optimizer_with_attributes(CS.Optimizer, "all_solutions"=>all_solutions,
                                                      "logging"=>[],
                                                      "traverse_strategy"=>:BFS,
                                                      # "traverse_strategy"=>:DFS,
                                                      # "traverse_strategy"=>:DBFS,

                                                      "branch_split"=>:Smallest,
                                                      # "branch_split"=>:Biggest,
                                                      # "branch_split"=>:InHalf,

                                                      # "simplify"=>false,
                                                      "simplify"=>true,
                                                      "time_limit"=>timeout
                                        ))





    problem = resize_matrix(problem) # Resize to "proper" matrix form
    # display(@show problem)
    # print_grid(problem)

    rows,cols = size(problem)
    println("rows:$rows cols:$cols")

    # @variable(m, 0 <= x[1:rows,1:cols] <= 1, Int)
    @variable(m, x[1:rows,1:cols], Bin)

    # Missing/unknown value
    M = -1
    for i in 1:rows, j in 1:cols
      # Check all that has a hint (i.e. number of surrounding mines)
      if problem[i,j] != M
           # 1) This cannot be a mine
          @constraint(m, x[i,j] == 0)

          # 2) problem[i,j] = the number of bombs in the neighborhood.
          s = sum([x[i+a,j+b] for a in -1:1 for b in -1:1
                            # if i+a > 0 && j+b > 0 && i+a <= rows && j+b <= cols
                            # if 1 <= i+a <= rows && 1 <= j+b <= cols
                            if i+a in 1:rows && j+b in 1:cols
                  ])
          @constraint(m, problem[i,j] == s)
      end
    end

    optimize!(m)
    status = JuMP.termination_status(m)
    println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(m, MOI.ResultCount())
        println("num_sols:$num_sols")
        for sol in 1:num_sols
            s = convert.(Integer,JuMP.value.(x,result=sol))
            print_grid(s)
            # join(replace(replace(s,0=>"X"),1=>"_"),"")|>println
        end
        println("num_sols:$num_sols")
        println("\n")

    end

end



all_problems = all_minesweeper_problems

all_solutions = true
print_solutions = true
timeout = 3
println("all_solutions:$all_solutions print_solutions:$print_solutions timeout:$timeout")

# for p in 12
for p in sort(collect(keys(all_problems)))
    println("\n\nproblem $p")
    @time minesweeper(all_problems[p],all_solutions,print_solutions,timeout)

end
