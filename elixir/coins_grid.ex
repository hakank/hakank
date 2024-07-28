# 
# Coins grid problem in Elixir.
#
# Problem from 
# Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
# http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
# """
# In a quadratic grid (or a larger chessboard) with 31x31 cells, one 
# should place coins in such a way that the following conditions are 
# fulfilled:
#   1. In each row exactly 14 coins must be placed.
#   2. In each column exactly 14 coins must be placed.
#   3. The sum of the quadratic horizontal distance from the main
#      diagonal of all cells containing a coin must be as small as possible.
#   4. In each cell at most one coin can be placed.
#
#  The description says to place 14x31 = 434 coins on the chessboard 
#  each row containing 14 coins and each column also containing 14 coins.
# """
#
# Note: This problem is quite/very hard for (plain) CP solvers. A MIP solver solves
# the 14,31 problem in millis.
#
#
# Cf the MiniZinc model http://hakank.org/minizinc/coins_grid.mzn
#
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule CoinsGrid do
  
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent  
  alias CPSolver.Model
  alias CPSolver.Objective
  
  # import CPSolver.Constraint.Factory
  import CPSolver.Variable.View.Factory
    
  def main() do

    
    n = 7 # 31
    c = 3 # 14
    
    rs = 0..n-1
    x = for i <- rs do
          for j <- rs do
            IntVariable.new(0..1, name: "x[#{i},#{j}]")
          end
        end

    x_flatten = List.flatten(x)

    # To be minimized
    z = IntVariable.new(0..n*n*n, name: "z]")

    # quadratic horizontal distance
    # MiniZinc: z = sum(i,j in 1..n) (  x[i,j]*(abs(i-j))*(abs(i-j))  )
    sum_constraint = Sum.new(z, for i <- rs, j <- rs do
                                         mul(CPUtils.mat_at(x,i,j),abs(i-j)**2)
                                       end)

    
    # MiniZinc: forall(i in 1..n) (  sum(j in 1..n) (x[i,j]) = c  )
    row_constraints = for row <- x do
      Sum.new(c,row)
    end

    # MiniZinc: forall(j in 1..n) (  sum(i in 1..n) (x[i,j]) = c  )    
    col_constraints = for col <- CPUtils.transpose(x) do 
      Sum.new(c,col)      
    end

    constraints = ([sum_constraint] ++ row_constraints ++ col_constraints) # |> List.flatten
    # constraints = row_constraints ++ col_constraints
    
    model = Model.new(x_flatten ++ [z],
                      constraints,
                      # minimize z
                      objective: Objective.minimize(z)
    )
    
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
    |> Enum.map(fn s -> IO.puts("z: #{Enum.at(s,n*n)}")
                        Enum.take(s,n*n) |> CPUtils.print_matrix(n,n) end)
    
  end
  
end
