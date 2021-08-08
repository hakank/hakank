#=

  Jobs puzzle in ConstraintSolver.jl 

  This is a standard problem in Automatic Reasoning.
  
  From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
  """
  Jobs Puzzle
  
  There are four people:  Roberta, Thelma, Steve, and Pete.
  Among them, they hold eight different jobs.
  Each holds exactly two jobs.
  The jobs are chef, guard, nurse, clerk, police officer (gender 
  not implied), teacher, actor, and boxer.
  The job of nurse is held by a male.
  The husband of the chef is the clerk.
  Roberta is not a boxer.
  Pete has no education past the ninth grade.
  Roberta, the chef, and the police officer went golfing together.
 
  Question:  Who holds which jobs?
  """
 
 
  The answer:
  Chef       Thelma
  Guard      Roberta
  Nurse      Steve
  Clerk      Pete
  Police     Steve
  Teacher    Roberta
  Actor      Pete
  Boxer      Thelma
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")


function jobs_puzzle(print_solutions=true,all_solutions=true,timeout=6)

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
   num_people = 4
   num_jobs = 8
   people = 1:num_people
   Roberta, Thelma, Steve, Pete = people
   
   @variable(model, 1 <= Jobs[1:num_jobs] <= num_people, Int)
   Chef, Guard, Nurse, Clerk, PoliceOfficer, Teacher, Actor, Boxer = Jobs
   
   # Each holds exactly two jobs.
   for i in 1:4 
        count_ctr(model, Jobs, :(==), i, 2)
   end 
   
   # The job of nurse is held by a male.
   # (Nurse == Steve \/ Nurse == Pete),
   # either_eq(model, Nurse,Steve, Nurse,Pete)
   # is_member_of(model,Nurse, [Steve,Pete])
   b1 = @variable(model, binary=true)
   @constraint(model, b1 := {Nurse == Steve || Nurse == Pete})
   @constraint(model, b1 == 1)

   # @either_eq(model, :(Nurse==Steve), :(Nurse==Pete))
    
   # The husband of the chef is the clerk.
   # (Clerk == Steve   \/ Clerk == Pete),
   # either_eq(model, Clerk,Steve, Clerk,Pete)
   # is_member_of(model,Clerk, [Steve,Pete])
   b2 = @variable(model, binary=true)
   @constraint(model, b2 := {Clerk == Steve || Clerk == Pete})
   @constraint(model, b2 == 1)

   # (Chef  #= Roberta #\/ Chef #= Thelma),
   # either_eq(model, Chef,Roberta, Chef,Thelma)
   # is_member_of(model,Chef, [Roberta,Thelma])
   b3 = @variable(model, binary=true)
   @constraint(model, b3 := {Chef  == Roberta || Chef == Thelma})
   @constraint(model, b3 == 1)

   @constraint(model, Chef != Clerk)

   # Roberta is not a boxer.
   @constraint(model, Roberta != Boxer)

   # Pete has no education past the ninth grade.
   @constraint(model, Pete != Teacher)
   @constraint(model, Pete != PoliceOfficer)
   @constraint(model, Pete != Nurse)

   # Roberta, [and] the chef, and the police officer 
   # went golfing together.
   @variable(model,Roberta <= RobertaVar <= Roberta, Int)
   @constraint(model, [RobertaVar,Chef,PoliceOfficer] in CS.AllDifferent())

   # From the name of the job
   # (Actor == Steve \/ Actor == Pete)
   # either_eq(model, Actor,Steve, Actor,Pete)
   # is_member_of(model,Actor, [Steve,Pete])
   b4 = @variable(model, binary=true)
   @constraint(model, b4 := {(Actor == Steve || Actor == Pete)})
   @constraint(model, b4 == 1)



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
                jobs_val = convert.(Integer,JuMP.value.(Jobs; result=sol))
                println("jobs:$jobs_val")

            end
        end
    else
        println("status:$status")
    end

    return status, num_sols
end

@time jobs_puzzle(true,true)
