#=

  Crypto problem (alphametic) in Julia ConstraintSolver.jl 

  This is a standard alphametic problem in mathematical recreations, 
  constraint programming etc.
    
  From GLPK:s model cryto.mod.
 
  """
  This problem comes from the newsgroup rec.puzzle.
  The numbers from 1 to 26 are assigned to the letters of the alphabet.
  The numbers beside each word are the total of the values assigned to
  the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
  5, 9, 20 and 13, or any other combination that add up to 47).
  Find the value of each letter under the equations:
 
  BALLET  45     GLEE  66     POLKA      59     SONG     61
  CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
  CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
  FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
  FUGUE   50     OPERA 65     SOLO       37     WALTZ    34
 
  Solution:
  A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
  5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18
 
  Reference:
  Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
  Simple problems, the crypto-arithmetic puzzle ALPHACIPHER.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function crypto(print_solutions=true,all_solutions=true)

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

                                                            "time_limit"=>6,

                                                            # "backtrack" => false, # default true
                                                            # "backtrack_sorting" => false, # default true

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    BALLET     =  45
    CELLO      =  43
    CONCERT    =  74
    FLUTE      =  30
    FUGUE      =  50
    GLEE       =  66
    JAZZ       =  58
    LYRE       =  47
    OBOE       =  53
    OPERA      =  65
    POLKA      =  59
    QUARTET    =  50
    SAXOPHONE  = 134
    SCALE      =  51
    SOLO       =  37
    SONG       =  61
    SOPRANO    =  82
    THEME      =  72
    VIOLIN     = 100
    WALTZ      =  34

    n = 26
    @variable(model, 1 <= x[1:n] <= n, Int)
    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z = x 

    @constraint(model, x in CS.AllDifferent())

    @constraint(model,B + A + L + L + E + T == BALLET)
    @constraint(model,C + E + L + L + O == CELLO)
    @constraint(model,C + O + N + C + E + R + T == CONCERT)
    @constraint(model,F + L + U + T + E == FLUTE)
    @constraint(model,F + U + G + U + E == FUGUE)
    @constraint(model,G + L + E + E == GLEE)
    @constraint(model,J + A + Z + Z == JAZZ)
    @constraint(model,L + Y + R + E == LYRE)
    @constraint(model,O + B + O + E == OBOE)
    @constraint(model,O + P + E + R + A == OPERA)
    @constraint(model,P + O + L + K + A == POLKA)
    @constraint(model,Q + U + A + R + T + E + T == QUARTET)
    @constraint(model,S + A + X + O + P + H + O + N + E == SAXOPHONE)
    @constraint(model,S + C + A + L + E == SCALE)
    @constraint(model,S + O + L + O == SOLO)
    @constraint(model,S + O + N + G == SONG)
    @constraint(model,S + O + P + R + A + N + O == SOPRANO)
    @constraint(model,T + H + E + M + E == THEME)
    @constraint(model,V + I + O + L + I + N == VIOLIN)
    @constraint(model,W + A + L + T + Z == WALTZ)

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
                println("x:$x_val")

            end
        end
    else
        println("status:$status")
    end

    return status
end

@time crypto()
