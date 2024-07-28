# 
# Subset sum problem in Elixir.
#
# From Katta G. Murty: "Optimization Models for Decision Making", page 340
# http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
#
# """
# Example 7.8.1

# A bank van had several bags of coins, each containing either
# 16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
# street, thieves stole some bags. A total of 100 coins were lost.
# It is required to find how many bags were stolen.
# """
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SubsetSum do
  
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  # alias CPSolver.Constraint.Sum  
  # alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.Circuit
  # alias CPSolver.Constraint.Element    
  # alias CPSolver.Constraint.NotEqual
  # alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  # alias CPSolver.Objective

  # Defines:
  # add/2-3,element/2-3,element2d/3-4,mod/2-3,
  # subtract/2-3,sum/1-2
  # (Note: add/2 is also defined in CPSolver.Variable.View.Factory)
  # import CPSolver.Constraint.Factory

  # Defines: add/2,linear/3,minus/1,mul/2
  # (Note: add/2 is also defined in CPSolver.Constraint.Factory)
  # import CPSolver.Variable.View.Factory


   def main() do

    total = 100
    coins = [16, 17, 23, 24, 39, 40]
    n = length(coins)

    x = for i <- 0..n-1, do: IntVariable.new(0..n, name: "x[#{i}]")

    model = Model.new(x,
      CPUtils.scalar_product(x,coins,total)
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
    for sol <- res.solutions do
      IO.inspect(sol |> Enum.take(n), label: "Solution")
    end
    
  end
   
end
