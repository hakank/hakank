#=

  Some examples of the regular global constraint in Julia ConstraintSolver.jl

  Here's a description of the transition tables.
  Note that input can never be 0 since it is used for no state.

  Example: regexp 123*21

     Transition = [
       # inputs: 1  2  3
                [2, 0, 0], # transitions from state 1: -2-> state 2  [start]
                [0, 3, 0], # transitions from state 2: -3-> state 3 #
                [0, 4, 3], # transitions from state 3: -3-> state 3, -2-> state 4
                [5, 0, 0], # transitions from state 4: -1-> state 5  [final]
                [0, 0, 0]  # transitions from state 5: (none)
                ],
     InitialState = 1,
     AcceptingStates = [5]

  (This is the first example, :1, below.)

  The states are numbered from 1..Transition.length.
  The initial state is in InitialState, i.e. the state Transition[InitialState].
  The columns are the inputs (the column index), e.g. the first state ([2, 0, 0])
  means that it only accept a "1" (first column), and then go to state 2 ([0, 3, 0],
  which only accept a "2" (second column) and go to state 3 ([0, 4, 3]) with the
  following meaning:

      - if "1": not accepted
      - if "2": goto state 4
      - if "3": goto state 3 (loop)

  And the DFA now go through the states. The accepting state in this
  example is only state 5 (AcceptingStates must be a list).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

#
# the regular constraints is in constraints_utils.jl
#
function regular_test(problem,n=4,print_solutions=true,all_solutions=true)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

    model = Model(optimizer_with_attributes(CS.Optimizer,   "all_solutions"=> all_solutions,
                                                            # "all_optimal_solutions"=>true,
                                                            "logging"=>[],

                                                            # "traverse_strategy"=>:BFS,
                                                            # "traverse_strategy"=>:DFS,
                                                            "traverse_strategy"=>:DBFS,

                                                            # "branch_split"=>:Smallest,
                                                            "branch_split"=>:Biggest,
                                                            # "branch_split"=>:InHalf,

                                                            # "simplify"=>false,
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>6,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    transition       = problem[:transition]
    n_states          = problem[:n_states]
    input_max        = problem[:input_max]
    initial_state    = problem[:initial_state]
    accepting_states = problem[:accepting_states]

    @variable(model, 1 <= x[1:n] <= 3, Int)

    a = regular(model,x,n_states,input_max,transition,initial_state,accepting_states)



    # @objective(model,Max,z)

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
                x_val = convert.(Integer,JuMP.value.(x; result=sol))
                a_val = convert.(Integer,JuMP.value.(a; result=sol))
                println("x:$x_val")
                println("a:$a_val")
                println()

            end
        end
    else
        println("status:$status")
    end

    return status
end

regular_problems = Dict(

#
# regexp 123*21
#
:1 => Dict(
   :info => "regex: 123*21",
   :transition => resize_matrix([
                [2, 0, 0], # transitions from state 1: -2-> state 2
                [0, 3, 0], # transitions from state 2: -3-> state 3 #
                [0, 4, 3], # transitions from state 3: -3-> state 3, -2-> state 4
                [5, 0, 0], # transitions from state 4: -1-> state 5
                [0, 0, 0]  # transitions from state 5: (none)
                ]),
   :n_states => 5,
   :input_max => 3,
   :initial_state => 1,
   :accepting_states => [5],
   :lens => 4:10, # The lengths to check ('n' in the model)
   :all_solutions => true, # Find all solutions?
   ),

# This example is from
# "Handbook of Constraint programming", page 180
#
#    ^(c+|a+b+a+){n}$
#    ^(3+|1+2+1+){n}$
#
# Or:  aa*bb* + cc*
#
# "It accepts the strings 'aaabaa' and 'cc', but not 'aacbba'."
#
:2 => Dict(
  :info => "It accepts the strings 'aaabaa' and 'cc', but not 'aacbba'.",
  :transition => resize_matrix([
      # inputs
      # 1 2 3
       [2,0,5], # 1 start
       [2,3,0], # 2 loop + next
       [4,3,0], # 3 next + loop
       [4,0,0], # 4 loop + end state
       [0,0,5]  # 5 loop and end state
     ]),
  :n_states => 5,
  :input_max => 3,
  :accepting_states => [4,5],
  :initial_state => 1,
  :lens => [4,5],
  :all_solutions => true,
 ),

#
# Example1 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
#
# First example
#        t.add(new Transition(0, 1, 1));
#        t.add(new Transition(1, 1, 2));
#        t.add(new Transition(2, 1, 3));
#
#        t.add(new Transition(3, 3, 0));
#        t.add(new Transition(0, 3, 0));
#
:3  => Dict(
  :info => "From Choco I",
  :transition => resize_matrix([
    [2, 0, 1], # 1
    [3, 0, 0], # 2
    [0, 4, 0], # 3
    [0, 0, 1]  # 4
   ]),
  :n_states => 4,
  :input_max => 3,
  :initial_state => 1,
  :accepting_states => [4],
  :lens => [10],
  :all_solutions => true,
),

#
# Example2 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
#
# This is an all_equal constraint. The initial_state decides which
# values it will be

:4 => Dict(
  :info => "From Choco 2. All equal constraint. initial_state (here 3) decides the value.",
  :transition => resize_matrix([
     [1, 0, 0, 0], # 1 (start)
     [0, 2, 0, 0], # 2
     [0, 0, 3, 0], # 3
     [0, 0, 0, 4]  # 4
   ]),
  :n_states => 4,
  :input_max => 4,
  :initial_state => 3,
  :accepting_states => 1:4,
  :lens => 3:6,
  :all_solutions => true,

),


#
# Example3 from Choco http://www.emn.fr/x-info/choco-solver/doku.php?id=regular
#
# This is stretchPath, not regular
#  lgt.add(new int[]{2, 2, 2}); // stretches of value 2 are exactly of size 2
#  lgt.add(new int[]{0, 2, 2}); // stretches of value 0 are exactly of size 2
#  lgt.add(new int[]{1, 2, 3}); // stretches of value 1 are at least of size 2 and at most 3
#
# This should be interpreted as:
#  0{2}1{2,3}2{2}
# for all permutations of 0,1,and 2.
# Or rather:
#  1{2}2{2,3}3{2}
# for all permutations of 1,2, and 3 since 0 is not allowed as an input value...
#
:5 => Dict(
  :info => "stretch_path: 1{2}2{2,3}3{2} for all permutations of 1,2, and 3",
  :transition => resize_matrix([
     [2, 4, 7], # 1 (start)
     [3, 0, 0], # 2
     [0, 4, 7], # 3 (end)
     [0, 5, 0], # 4
     [2, 6, 7], # 5 (end)
     [2, 0, 7], # 6 (end)
     [0, 0, 8], # 7
     [2, 4, 0]  # 8 (end)
   ]),
  :n_states => 8,
  :input_max => 3,
  :initial_state => 1,
  :accepting_states => [3,5,6,8],
  :lens => 4:6,
  :all_solutions => true,
),

#
# All different.
# This works in principle (here is n=3), but the automaton will
# be forbiddingly large for larger arrays.
#
:6 => Dict(
  :info => "all_different (size 3)",
  :transition => resize_matrix([
     #  1   2   3
      [ 2,  7, 12], # 1 (start)
      [ 0,  3,  5], # 2
      [ 0,  0,  4], # 3
      [ 0,  0,  0], # 4
      [ 0,  6,  0], # 5
      [ 0,  0,  0], # 6
      [ 8,  0, 10], # 7
      [ 0,  0,  9], # 8
      [ 0,  0,  0], # 9
      [11,  0,  0], # 10
      [ 0,  0,  0], # 11
      [13, 15,  0], # 12
      [ 0, 14,  0], # 13
      [ 0,  0,  0], # 14
      [ 16, 0,  0], # 15
      [ 0,  0,  0]  # 16
   ]),
  :n_states => 16,
  :input_max => 3,
  :initial_state => 1,
  :accepting_states => [4,6,9,11,14,16],
  :lens => [3],
  :all_solutions => true,
 ),


#
# "Need Regular Expression for Finite Automata: Even number of 1s and Even number of 0s"
# http://stackoverflow.com/questions/17420332/need-regular-expression-for-finite-automata-even-number-of-1s-and-even-number-o
#
#     Q1 <- 1 -> Q2
#     ^          ^
#     |          |
#     2          2
#     |          |
#     v          v
#     Q4 <- 1 -> Q3
#
:7 => Dict(
  :info => "Even number of 1s and even number of 0s",
  :transition => resize_matrix([
     #  1   2
      [ 2,  4], # state 1 Q1 start, final
      [ 1,  3], # state 2 Q2
      [ 4,  2], # state 3 Q3
      [ 3,  1]  # state 4 Q4
   ]),
  :n_states => 4,
  :input_max => 2,
  :initial_state => 1,
  :accepting_states => [1],
  :lens => 1:6, # must be an even number
  :all_solutions=> true
  ),

)

# Test the problems
for p in sort(collect(keys(regular_problems)))
    println("\n\nPROBLEM $p")
    problem = regular_problems[p]
    local all_solutions = get(problem,:all_solutions,false)
    lens = get(problem,:lens, 5)
    info = get(problem,:info, "<no info")
    println("Info: $info")
    for n in lens
        println("\nn:$n")
        @time regular_test(regular_problems[p],n,true,all_solutions)
    end
end
