#=

  A Round of Golf puzzle in Julia ConstraintSolver.jl

  From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
  """
  Title: A Round of Golf
  Author: Ellen K. Rodehorst
  Publication: Dell Favorite Logic Problems
  Issue: Summer, 2000
  Puzzle #: 9
  Stars: 1

  When the Sunny Hills Country Club golf course isn't in use by club members,
  of course, it's open to the club's employees. Recently, jack and three other
  workers at the golf course got together on their day off to play a round of
  eighteen holes of golf.
  Afterward, all four, including Mr. green, went to the clubhouse to total
  their scorecards. Each man works at a different job (one is a short-order
  cook), and each shot a different score in the game. No one scored below
  70 or above 85 strokes. From the clues below, can you discover each man's
  full name, job and golf score?

  1. bill, who is not the maintenance man, plays golf often and had the lowest
  score of the foursome.
  2. Mr. clubb, who isn't paul, hit several balls into the woods and scored ten
  strokes more than the pro-shop clerk.
  3. In some order, frank and the caddy scored four and seven more strokes than
  Mr. sands.
  4. Mr. carter thought his score of 78 was one of his better games, even
     though frank's score  was lower.
  5. None of the four scored exactly 81 strokes.

  Determine: First Name - Last Name - Job - score
  """

  Compare with the F1 model:
  http://www.f1compiler.com/samples/A 20Round 20of 20Golf.f1.html

  Solution:
             jack, bill, paul, frank
             clubb sands carter green
             maint cook  caddy clerk
             85    71    78    75
  first_name: [1, 2, 3, 4]
  last_name : [4, 1, 2, 3]
  job       : [2, 1, 4, 3]
  score     : [85, 71, 78, 75]

  Note: This model is quite messy since ConstraintSolver.jl v0.5.3 don't
  have support for a good element (x[y] == z) and a little limited form
  of indicator/reification. Ole is working on all of this as we speak... :-)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function a_round_of_golf(print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            # "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            # "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>3,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    n = 4
    jack,bill,paul,frank = 1:n
    first_name = [jack,bill,paul,frank]

    @variable(model, 1 <= last_name[1:n] <= n, Int)
    green,clubb,sands,carter = last_name

    @variable(model, 1 <= job[1:n] <= n, Int)
    cook,maintenance_man,clerk,caddy = job

    @variable(model, 70 <= score[1:n] <= 85, Int)
    score_jack,score_bill,score_paul,score_frank = score

    @constraint(model, last_name in CS.AllDifferent())
    @constraint(model, job in CS.AllDifferent())
    @constraint(model, score in CS.AllDifferent())

    #  1. bill, who is not the maintenance man, plays golf often and had
    #     the lowest score of the foursome.
    @constraint(model, bill != maintenance_man)

    @constraint(model, score_bill < score_jack)
    @constraint(model, score_bill < score_paul)
    @constraint(model, score_bill < score_frank)


    #  2. Mr. clubb, who isn"t paul, hit several balls into the woods and
    #     scored ten strokes more than the pro-shop clerk.
    @constraint(model, clubb != paul)

    @variable(model, 70 <= score_clubb <= 85, Int)
    @variable(model, 70 <= score_clerk <= 85, Int)
    my_element(model,clubb,score,score_clubb)
    my_element(model,clerk,score,score_clerk)
    @constraint(model, score_clubb == score_clerk + 10)


    #  3. In some order, frank and the caddy scored four and seven more
    #     strokes than Mr. sands.
    @constraint(model, frank != caddy)
    @constraint(model, frank != sands)
    @constraint(model, caddy != sands)

    @variable(model, 70 <= score_sands <= 85, Int)
    @variable(model, 70 <= score_caddy <= 85, Int)
    @variable(model, 70 <= score_carter <= 85, Int)
    my_element(model, sands,score,score_sands)
    my_element(model, caddy,score,score_caddy)
    my_element(model, carter,score,score_carter)
    
    #=
    # Picat code:
    (
       (scorefrank = scoresands + 4 #/\
       scorecaddy = scoresands + 7)
       #\/
       (scorefrank = scoresands + 7 #/\
       scorecaddy = scoresands + 4)
    ),
    =#
    #= 
    c1 = @variable(model, binary=true )
    @constraint(model, c1 := {score_frank == score_sands + 4 && score_caddy == score_sands + 7} )
    c2 = @variable(model, binary=true )
    @constraint(model, c2 := {score_frank == score_sands + 7 && score_caddy == score_sands + 4} )
    @constraint(model, c1 + c2 == 1)
    =# 
    @constraint(model, (score_frank == score_sands + 4 && score_caddy == score_sands + 7) || 
                        (score_frank == score_sands + 7 && score_caddy == score_sands + 4)   
                       )


    #  4. Mr. carter thought his score of 78 was one of his better games, even
    #  though frank"s score was lower.
    @constraint(model,frank != carter)

    #  score[carter] = 78,
    @constraint(model, score_carter == 78)
    @constraint(model, score_frank < score_carter)

    #  5. None of the four scored exactly 81 strokes.
    for s in score
        @constraint(model, s != 81)
    end

    # Solve the problem
    optimize!(model)

    status = JuMP.termination_status(model)
    if status == MOI.OPTIMAL
        num_sols = MOI.get(model, MOI.ResultCount())
        println("num_sols:$num_sols\n")
        if print_solutions
            for sol in 1:num_sols
                println("solution #$sol")
                last_name_val = convert.(Integer,JuMP.value.(last_name; result=sol))
                job_val = convert.(Integer,JuMP.value.(job; result=sol))
                score_val = convert.(Integer,JuMP.value.(score; result=sol))
                println("last_name:$last_name_val")
                println("job      :$job_val")
                println("score    :$score_val")
                println()
            end
        end
    else
        println("status:$status")
    end

    return status
end

@time a_round_of_golf()
