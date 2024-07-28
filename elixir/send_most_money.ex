# 
# SEND+MOST=MONEY optimization problem in Elixir.
#
# Alphametic problem were we maximize MONEY.
#
# This version is doing two things:
#  - find the maximum of MONEY
#  - and then find all solutions for the maximum value of MONEY.
#
# Problem from the lecture notes:
# http://www.ict.kth.se/courses/ID2204/notes/L01.pdf
#
# There are two maximal solutions:
#  9782 + 1094 = 10876
#  9784 + 1092 = 10876

# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SendMostMoney do
  
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  alias CPSolver.Constraint.Sum  
  # alias CPSolver.Constraint.LessOrEqual
  alias CPSolver.Constraint.NotEqual
  alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  alias CPSolver.Objective
  
  import CPSolver.Constraint.Factory
  import CPSolver.Variable.View.Factory
    
  def main() do
    max_val = send_most_money()
    IO.puts("Found max_val: #{max_val}")
    send_most_money(max_val)
  end

  def send_most_money(max_val \\ nil) do
    n = 8
    xs = for i <- 0..n-1 do IntVariable.new(0..9, name: "x[#{i}]") end
    [s,e,n,d,m,o,t,y] = xs

    {send_var,send_constraint} = sum([mul(s,1000),mul(e,100),mul(n,10),d])
    {most_var,most_constraint} = sum([mul(m,1000),mul(o,100),mul(s,10),t])
    # To maximize
    {money_var,money_constraint} = sum([mul(m,10_000),mul(o,1000),mul(n,100),mul(e,10),y])        

    max_val_constraint = max_val && Equal.new(money_var,max_val) || Equal.new(money_var,money_var)

    constraints = [send_constraint,most_constraint,money_constraint,max_val_constraint,
                   AllDifferent.new(xs),
                   Sum.new(money_var,[send_var,most_var]), # SEND+MOST = MONEY
                   NotEqual.new(s,0),NotEqual.new(m,0)
                  ]
    vars = xs++[money_var,send_var,most_var]
    
    model = max_val == nil && Model.new(vars, constraints,objective: Objective.maximize(money_var))
                           || Model.new(vars, constraints)
    
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
    
    sols = if max_val == nil do [res.solutions |> List.last] else res.solutions end
    sols
    |> Enum.map(fn s -> 
      money_val = Enum.at(s,8)
      send_val = Enum.at(s,9)
      most_val = Enum.at(s,10)
      :io.format("~w + ~w = ~w~n",[send_val,most_val,money_val])
    end)

    if max_val == nil do
      Enum.at(sols |> hd,8)
    end
    
  end
  
end
