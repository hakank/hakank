# 
# Seseman problem in Elixir.
#
#  Description of the problem:
#  
# n is the length of a border
# There are (n-2)^2 "holes", i.e.
# there are n^2 - (n-2)^2 variables to find out.
#
# The simplest problem, n = 3 (n x n matrix)
# which is represented by the following matrix:
#
#  a b c 
#  d   e 
#  f g h 
#
# Where the following constraints must hold:
#
#   a + b + c = border_sum
#   a + d + f = border_sum
#   c + e + h = border_sum
#   f + g + h = border_sum
#   a + b + c + d + e + f = total_sum
#
# For a (Swedish) discussion of this problem, see
# "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
# http://www.hakank.org/webblogg/archives/001084.html
# and
# Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
# (using ECLiPSe CLP code)
#
# It was also is commented in the (Swedish) blog post
# "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
# http://www.hakank.org/webblogg/archives/001209.html
#
# It should be 85 solutions. And it does:
#
# 5 2 2
# 3   3
# 1 4 4
#
# 5 1 3
# 1   5
# 3 5 1
#
# 4 2 3
# 1   5
# 4 4 1
#
# 3 2 4
# 2   4
# 4 4 1
#
# ...
#
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule Seseman do
  
  # import Enum # Conflicts with CPSolver.Constraint.Factory.sum
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal 
  alias CPSolver.Model
  
  # import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory

  def print_solution(x) do
    :io.format("~w ~w ~w~n~w   ~w~n~w ~w ~w~n~n", x)
  end
 
  def main() do
    
    Logger.configure(level: :info)
    
    rowsum = 9
    total = 24

    # It should be 84 solutions
    
    # Decision variables
    x = Enum.map(0..7,fn i -> IntVariable.new(1..9, name: "x[#{i}]") end)
    [a,b,c,
     d,  e,
     f,g,h] = x
   
    # The different sums that should add to rowsum
    ts = [ [a,b,c],
           [a,d,f],
           [c,e,h],
           [f,g,h]
         ]
    row_sum_constraints = for t <- ts, do: Sum.new(rowsum,t)
    total_constraint = Sum.new(total,x)

    model = Model.new(x,
       [total_constraint | row_sum_constraints ]
     )

    {:ok, result} =
      CPSolver.solve_sync(model,
        search: {:first_fail, :indomain_max},
        # stop_on: {:max_solutions, 3}, # It should be 85 solutions
        timeout: :infinity,
        space_threads: 12
      )    
    
    result.solutions
    |> Enum.map(fn s -> s |> Enum.take(8) |> print_solution end)
    
    IO.inspect(result.statistics)    
    
  end
  
end
