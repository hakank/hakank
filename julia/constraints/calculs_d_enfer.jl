#=

  Calculs d'enfer puzzle in Julia ConstraintSolver.jl

  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html

  The solution is the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.

  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """

  Also, see the discussion of the Z model:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  (which shows the same solution).

  This model
  a_max:16
  aa:[-13, -2, -14, -16, -10, 4, 13, -1, -3, -12, -11, 16, -9, 11, 0, -8, -6, 15, 9, 2, -15, 14, -7, 7, -4, -5]
  11.144555 seconds (29.14 M allocations: 5.421 GiB, 15.63% gc time)


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
const CS = ConstraintSolver
include("constraints_utils.jl")

function calculs_d_enfer(print_solutions=true,all_solutions=false)

    cbc_optimizer = optimizer_with_attributes(Cbc.Optimizer, "logLevel" => 0)
    glpk_optimizer = optimizer_with_attributes(GLPK.Optimizer)
    ipopt_optimizer = optimizer_with_attributes(Ipopt.Optimizer)

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
                                                            # "simplify"=>true, # default

                                                            "time_limit"=>13,

                                                            # "lp_optimizer" => cbc_optimizer,
                                                            # "lp_optimizer" => glpk_optimizer,
                                                            # "lp_optimizer" => ipopt_optimizer,
                                        ))

    nn = 26
    @variable(model, -100 <= aa[1:nn] <= 100, Int)
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t,s,u,v,w,x,y,z = aa

    @variable(model, 0 <= a_abs[1:nn] <= 100, Int)
    for ii in 1:nn
        my_abs(model,aa[ii],a_abs[ii])
    end

    @variable(model, 0 <= a_max <= nn,Int)
    my_max(model, a_abs, a_max)

    @constraint(model, aa in CS.AllDifferentSet())

    @constraint(model, z+e+r+o     == 0)
    @constraint(model, o+n+e       == 1)
    @constraint(model, t+w+o       == 2)
    @constraint(model, t+h+r+e+e   == 3)
    @constraint(model, f+o+u+r     == 4)
    @constraint(model, f+i+v+e     == 5)
    @constraint(model, s+i+x       == 6)
    @constraint(model, s+e+v+e+n   == 7)
    @constraint(model, e+i+g+h+t   == 8)
    @constraint(model, n+i+n+e     == 9)
    @constraint(model, t+e+n       == 10)
    @constraint(model, e+l+e+v+e+n == 11)
    @constraint(model, t+w+e+l+f   == 12)

    @objective(model,Min,a_max)

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
                aax = convert.(Integer,JuMP.value.(aa; result=sol))
                a_maxx = convert.(Integer,JuMP.value.(a_max; result=sol))
                println("a_max:$a_maxx")
                println("aa:$aax")

            end
        end
    else
        println("status:$status")
    end

    return status
end

@time calculs_d_enfer()
