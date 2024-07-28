# 
# Global constraint increasing/1 in Elixir.
#
# Test of the increasing/1 constraint
# 
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule IncreasingTest do
  
  # import CPUtils
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  # alias CPSolver.Constraint.Sum  
  # alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.NotEqual
  # alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  # alias CPSolver.Objective
  
  # import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory

  def main() do
    
    n = 5
    x = for i <- 0..n-1 do IntVariable.new(0..n, name: "x[#{i}]") end

    constraints = CPUtils.increasing(x)
    
    # constraints = CPUtils.decreasing(x) 
    
    model = Model.new(x, constraints)
    
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
    # IO.inspect(res.statistics)
    
    res.solutions
    |> IO.inspect
    
  end
  
end
