#=

  Labeled dice and Building blocks problems in Julia ConstraintSolver.jl

  Labeled dice
  --------------
  From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  """
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY,
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question
  is:  can one assign the 24 letters to 4 different cubes so that the
  four letters of each word appears on different cubes.  (There is one
  letter from each word on each cube.)  It might be fun for you to try
  it.  I'll give a small hint at the end of this post. The puzzle was
  created by Humphrey Dudley.
  """

  Building blocks
  ---------------
  From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
  """
  Each of four alphabet blocks has a single letter of the alphabet on each
  of its six sides. In all, the four blocks contain every letter but
  Q and Z. By arranging the blocks in various ways, you can spell all of
  the words listed below. Can you figure out how the letters are arranged
  on the four blocks?

  BAKE ONYX ECHO OVAL
  GIRD SMUG JUMP TORN
  LUCK VINY LUSH WRAP
  """

  Note: This is a somewhat generalized model for solving both
        Building blocks and Labeled Dice problems.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function block_problem(problem,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # https://wikunia.github.io/ConstraintSolver.jl/stable/options/#branch_strategy-(:Auto)
                                                            "branch_strategy" => :IMPS, # default
                                                            # "branch_strategy" => :ABS, # Activity Based Search
                                                            "activity.decay" => 0.999, # default 0.999
                                                            "activity.max_probes" => 3, # default, 10
                                                            "activity.max_confidence_deviation" => 3, # default 20

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))
    # a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z = 1:26
    println("problem ", problem[:name])
    num_cubes = problem[:num_cubes]
    num_sides = problem[:num_sides]
    letters = split(problem[:letters],"")
    num_chars = length(letters)

    # letters = problem[:letters]
    words = problem[:words]
    num_words = length(words)

    d = Dict(zip(letters,1:length(letters)))
    d_rev = [k=>v for (v,k) in d] # reversed dict

    cube_len = num_cubes*num_sides
    @variable(model, 1 <= cube[1:cube_len] <= num_cubes, Int)

    # each letters in a word must be on a different die
    for i in 1:num_words
      word = [d[w] for w in split(words[i,:][1],"")]
      this_die = @variable(model, [1:length(word)],CS.Integers(1:num_cubes))
      for i in 1:length(word)
          my_element(model,word[i],cube,this_die[i])
      end
      @constraint(model, this_die in CS.AllDifferentSet())
    end

    # there must be exactly NumSides (6) letters of each die (num_cubes)
    global_cardinality_count(model, cube, [num_sides for _ in 1:num_cubes])

    # simple symmetry breaking: place first letter on cube 1
    @constraint(model, cube[1] == 1)

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    # println("status:$status")
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("\nsolution #$sol")
                cube_val = convert.(Integer,JuMP.value.(cube; result=sol))
                println("cube:$cube_val")
                println("words:")
                for word in words
                    print(word,": ")
                    for w in split(word,"")
                        dd = d[w]
                        print("$w: $(cube_val[dd]) ")
                    end
                    println()
                end
                println("\ndice:")
                dice = [ [] for _ in 1:num_cubes]
                for (c,i) in collect(zip(letters,cube_val))
                    push!(dice[i],c)
                end
                join.(sort.(unique.(dice))," ").|>println
            end
        end
    else
        println("status:$status")
    end

    return status
end

function block_problems(problem)
    all_block_problems = Dict(
      :labeled_dice =>  Dict(
           :name => "labeled_dice",
           # number of cubes
           :num_cubes => 4,
           # number of sides of of a cube
           :num_sides => 6,
           # the letters to use
           :letters => "abcdefghijklmnopqrstuvwy",
           # the words to place
           :words => ["buoy", "cave", "celt", "flub",
                      "fork", "hemp", "judy","junk",
                      "limn", "quip", "swag", "visa",
                      "wish"]
    ),

    :building_blocks => Dict(
           :name => "building_blocks",
           :num_cubes => 4,
           :num_sides => 6,
           # Note: 'f' is not in any word but it needed to get 24 chars
           :letters => "abcdefghijklmnoprstuvwxy",
           :words => ["bake", "onyx", "echo", "oval",
                      "gird", "smug", "jump", "torn",
                      "luck", "viny", "lush", "wrap"]
        )

   )

   return all_block_problems[problem]

end

@time block_problem(block_problems(:labeled_dice),true,true)
println()
@time block_problem(block_problems(:building_blocks),true,true)
