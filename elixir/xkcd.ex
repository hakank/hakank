# 
# XKCD subset-sum problem in Elixir.
#
# From http://xkcd.com/287/
#
# Some amount (or none) of each dish should be ordered to give a total
# of exact 15.05.
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule XKCD do
  
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent  
  alias CPSolver.Model
  # alias CPSolver.Objective
  
  # import CPSolver.Constraint.Factory
  import CPSolver.Variable.View.Factory
    
  def main() do

    # Using integers (2.15 -> 215)
    prices = [215, 275, 335, 355, 420, 580]
    total  = 1505

    len = length(prices)
    
    x = for i <- 0..len-1 do IntVariable.new(0..10, name: "x[#{i}]") end

    # constraints = [Sum.new(total, for i <- 0..len-1 do mul(Enum.at(x,i),Enum.at(prices,i)) end )]
    # constraints = [Sum.new(total, for {xi,pi} <- Enum.zip(x,prices) do mul(xi,pi) end )]
    constraints = CPUtils.scalar_product(x,prices,total)
    
    model = Model.new(x, constraints)
    
    Logger.configure(level: :info)

    opts = [
      search: {:first_fail, :indomain_max},
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
    res.solutions
    
  end
  
end
