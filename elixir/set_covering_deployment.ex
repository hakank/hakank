# 
# Set covering deployment in Elixir.
#
# From http://mathworld.wolfram.com/SetCoveringDeployment.html
# """
# Set covering deployment (sometimes written "set-covering deployment"
# and abbreviated SCDP for "set covering deployment problem") seeks 
# an optimal stationing of troops in a set of regions so that a 
# relatively small number of troop units can control a large 
# geographic region. ReVelle and Rosing (2000) first described 
# this in a study of Emperor Constantine the Great's mobile field 
# army placements to secure the Roman Empire.
# """
#
# Cf http://hakank.org/minizinc/set_covering_deployment.mzn
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SetCoveringDeployment do

  import CPUtils
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  # alias CPSolver.Constraint.Sum  
  alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.Circuit
  # alias CPSolver.Constraint.Element    
  # alias CPSolver.Constraint.NotEqual
  # alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  alias CPSolver.Objective

  # Defines:
  # add/2-3,element/2-3,element2d/3-4,mod/2-3,
  # subtract/2-3,sum/1-2
  # (Note: add/2 is also defined in CPSolver.Variable.View.Factory)
  import CPSolver.Constraint.Factory

  # Defines: add/2,linear/3,minus/1,mul/2
  # (Note: add/2 is also defined in CPSolver.Constraint.Factory)
  # import CPSolver.Variable.View.Factory


  # The connection matrix
  def problem(1) do
    [[0, 1, 0, 1, 0, 0, 1, 1],
     [1, 0, 0, 1, 0, 0, 0, 0],
     [0, 0, 0, 0, 1, 1, 0, 0],
     [1, 1, 0, 0, 0, 0, 1, 0],
     [0, 0, 1, 0, 0, 1, 1, 0],
     [0, 0, 1, 0, 1, 0, 1, 1],
     [1, 0, 0, 1, 1, 1, 0, 1],
     [1, 0, 0, 0, 0, 1, 1, 0]]
  end


  
  def main() do

    mat = problem(1)

    n = length(mat)
    
    # First army
    xs = for i <- 0..n-1 do IntVariable.new(0..1, name: "xs[#{i}]") end
    
    # Second army
    ys = for i <- 0..n-1 do IntVariable.new(0..1, name: "ys[#{i}]") end
    

    #
    # Constraint 1: There is always an army in a city (+ maybe a backup)
    #               Or rather: Is there a backup, there must be an
    #               an army
    # 
    constraint1 = for {x,y} <- Enum.zip(xs,ys), do: LessOrEqual.new(y,x)
    
    #
    # Constraint 2: There should always be an backup army near
    # every city
    #
    constraint2 = for {x,row} <- Enum.zip(xs,mat) do
      {s_var, s_cons} = sum(for {m,y} <- Enum.zip(row,ys) do CPSolver.Variable.View.Factory.mul(y,m) end)
      {add_var,add_cons} = add(x,s_var)
      leq_cons = LessOrEqual.new(1,add_var)
      [s_cons,leq_cons,add_cons]
    end
    |> List.flatten

    # Thanks to Boris Okner for this neat solution.
    all_armies = xs ++ ys
    # objective: minimize the number of armies    
    {sum_var, sum_constraint} = sum(all_armies)

    
    model = Model.new(all_armies,
     [sum_constraint | (constraint1 ++ constraint2) |> List.flatten ],
      objective: Objective.minimize(sum_var)
    )
    
    Logger.configure(level: :info)

    opts = [
      search: {:first_fail, :indomain_min},
      # search: {:input_order, :indomain_min},      
      # search: {:first_fail, :indomain_random},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
      ]      
    {:ok, res} = CPSolver.solve_sync(model,
                                     opts
                                     )
    IO.inspect(res.statistics)
    sol = res.solutions |> hd
    # IO.inspect(sol, label: "sol")
    
    # x_val = Enum.slice(sol,0,n)
    # y_val = Enum.slice(sol,n,n)
    # z_val = Enum.at(sol,n*2)        
    # IO.inspect(x_val, label: "x")
    # IO.inspect(y_val, label: "y")
    # IO.inspect(z_val, label: "z")
    IO.inspect(res.objective, label: "objective")
    xs_val = for i <- 0..n-1, do: get_solution_value(res,sol,"xs[#{i}]")
    ys_val = for i <- 0..n-1, do: get_solution_value(res,sol,"ys[#{i}]")
    IO.inspect(xs_val, label: "x")
    IO.inspect(ys_val, label: "y")
    # res
  end
  
end
