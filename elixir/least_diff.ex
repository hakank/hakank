# 
# Least diff problem in Elixir.
#
# What is the smallest difference between two numbers X - Y
# if you must use all the digits (0..9) exactly once, i.e.
# Minimize the difference 
#   ABCDE - FGHIJ
#
# Solution: 50123 - 49876 = 247
# 
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule LeastDiff do
  
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  alias CPSolver.Constraint.LessOrEqual
  alias CPSolver.Model
  alias CPSolver.Objective
  
  import CPSolver.Constraint.Factory
  import CPSolver.Variable.View.Factory
    
  def main() do

    n = 10
    xs = for i <- 0..n-1 do IntVariable.new(0..9, name: "x[#{i}]") end
    [a,b,c,d,e,  f,g,h,i,j] = xs

    # To minimize
    z = IntVariable.new(0..10**6, name: "z")

    # This is slower: 0.45s
    # x = IntVariable.new(0..10**6, name: "x")
    # y = IntVariable.new(0..10**6, name: "y")    
    # x_constraint = Sum.new(x, [mul(a,10_000),mul(b,1000),mul(c,100),mul(d,10),e])
    # y_constraint = Sum.new(y, [mul(f,10_000),mul(g,1000),mul(h,100),mul(i,10),j])
    
    # A little faster: 0.2s
    {x,x_constraint} = sum([mul(a,10_000),mul(b,1000),mul(c,100),mul(d,10),e])
    {y,y_constraint} = sum([mul(f,10_000),mul(g,1000),mul(h,100),mul(i,10),j])

   
    model = Model.new(xs++[x,y,z],
      [x_constraint,
       y_constraint,
       AllDifferent.new(xs),
       LessOrEqual.new(y,x),
       Sum.new(z,[x,mul(y,-1)])], # calculate z
      objective: Objective.minimize(z))
    
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
    sols = res.solutions |> List.last
    diff_val = Enum.at(sols,10)
    x_val = Enum.at(sols,11)
    y_val = Enum.at(sols,12)
    :io.format("~w - ~w = ~w~n",[x_val,y_val,diff_val])
    
    
  end
  
end
