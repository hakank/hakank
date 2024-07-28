# 
# Set covering problem in Elixir.
#
# Placing of firestations, from Winston "Operations Research", page 486
#
# Cf http://hakank.org/minizinc/set_covering.mzn
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SetCovering do
  
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  alias CPSolver.Constraint.Sum  
  alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.NotEqual
  # alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  alias CPSolver.Objective
  
  import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory

  def problem(1) do
    min_distance = 15                    # minimum distance 
    distance     = [[ 0,10,20,30,30,20],  # distances between the cities
                    [10, 0,25,35,20,10],
                    [20,25, 0,15,30,20],
                    [30,35,15, 0,15,25],
                    [30,20,30,15, 0,14],
                    [20,10,20,25,14, 0]]
    [min_distance,distance]
  end

  
  def main() do

    [min_distance, distance] = problem(1)

    num_cities = length(distance)
    
    # where to place the fire stations: 1 if placed in this city.
    x = for i <- 0..num_cities-1 do IntVariable.new(0..1, name: "x[#{i}]") end

    # number of fire stations, to minimize
    z = IntVariable.new(0..num_cities, name: "z")

    # calculate the number of covered fire stations
    constraints = for j <- 0..num_cities-1 do
                    {d_var, d_constraint} = sum(for i <- 0..num_cities-1,
                                            CPUtils.mat_at(distance,i,j) <= min_distance do Enum.at(x,i) end)
                    gt_constraint = LessOrEqual.new(1,d_var) # d >= 1
                    [gt_constraint,d_constraint]
                  end
    
    model = Model.new(x ++ [z] ,
      constraints ++ [ Sum.new(z, x) ] |> List.flatten,
      objective: Objective.minimize(z)
    )
    
    Logger.configure(level: :info)

    opts = [
      # search: {:first_fail, :indomain_min},
      search: {:input_order, :indomain_min},      
      # search: {:first_fail, :indomain_random},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
      ]      
    {:ok, res} = CPSolver.solve_sync(model,
                                     opts
                                     )
    IO.inspect(res.statistics)
    sol = res.solutions |> List.last

    covering = Enum.take(sol,num_cities)
    z_val = Enum.at(sol,num_cities)
    IO.inspect(z_val, label: "z")    
    IO.inspect(covering, label: "covering")    
  end
  
end
