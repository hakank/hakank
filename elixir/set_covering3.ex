# 
# Set covering problem in Elixir.
#
# Problem from 
# Katta G. Murty: "Optimization Models for Decision Making", page 302f
# http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
#
# 10 senators making a committee, where there must at least be one 
# representative from each group:
# group:        senators:
# southern      1 2 3 4 5
# northern      6 7 8 9 10
# liberals      2 3 8 9 10
# conservative  1 5 6 7
# democrats     3 4 5 6 7 9
# republicans   1 2 8 10
#
# The objective is to minimize the number of senators.

# Solution:
#   Found min_val:: 2
# 
#   Checking all optimal solutions:
#   Choosen: [:b, :f]
#   Choosen: [:e, :h]
#   Choosen: [:a, :i]
#   Choosen: [:b, :g]
#   Choosen: [:e, :j]
#
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SetCovering3 do
  
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  alias CPSolver.Constraint.Sum  
  alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.Circuit
  # alias CPSolver.Constraint.Element    
  # alias CPSolver.Constraint.NotEqual
  alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  alias CPSolver.Objective

  # Defines:
  # add/2-3,element/2-3,element2d/3-4,mod/2-3,
  # subtract/2-3,sum/1-2
  # (Note: add/2 is also defined in CPSolver.Variable.View.Factory)
  import CPSolver.Constraint.Factory

  # Defines: add/2,linear/3,minus/1,mul/2
  # (Note: add/2 is also defined in CPSolver.Constraint.Factory)
  import CPSolver.Variable.View.Factory
  
  #
  # The Belong matrix:
  #
  # 1 if a senator belongs to the group, 
  # 0 if senator don't belong to the group
  #
  def belongs() do
   [[1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   # 1 southern
    [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   # 2 northern
    [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   # 3 liberals
    [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   # 4 conservative
    [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   # 5 democrats
    [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]]   # 6 republicans
  end

  def senators do
    [:a,:b,:c,:d,:e,:f,:g,:h,:i,:j]
  end

  def main() do

    belongs = belongs()
    senators = senators()
    min_val = set_covering3(belongs,senators)
    IO.inspect(min_val, label: "Found min_val:")
    IO.puts("\nChecking all optimal solutions:")    
    set_covering3(belongs,senators,min_val)    
  end

  def set_covering3(belongs,senators, min_val \\ nil) do
    
    num_groups = length(belongs)
    num_senators = length(senators)

    # Which senator to choose
    x = for i <- 0..num_senators-1, do: IntVariable.new(0..1, name: "x[#{i}]")

    z = IntVariable.new(0..num_senators, name: "z")
    
    # cover all groups with the senators
    group_constraints = for i <- 0..num_groups-1 do
                        {sum_var, sum_cons} = sum(for j <- 0..num_senators-1 do
                                                    mul(Enum.at(x,j),CPUtils.mat_at(belongs,i,j))
                                                   end)
      leq = LessOrEqual.new(1,sum_var)
      [sum_cons,leq]
    end

    z_cons = Sum.new(z,x)
    
    all_vars        =  x ++ [z]    
    all_constraints = [z_cons | group_constraints] |> List.flatten

    model = min_val != nil && Model.new(all_vars, [ Equal.new(z,min_val) | all_constraints] |> List.flatten )
                           || Model.new(all_vars, all_constraints,
                                        objective: Objective.minimize(z))

    Logger.configure(level: :info)

    opts = [
      search: {:first_fail, :indomain_min},
      # search: {:input_order, :indomain_min},      
      # search: {:first_fail, :indomain_random},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
      ]      
    {:ok, res} = CPSolver.solve_sync(model, opts)

    for sol <- res.solutions do
      choosen = Enum.with_index(sol) 
                |> Enum.take(num_senators)
                |> Enum.filter(fn {s,_ix} -> s == 1 end)
                |> Enum.map(fn {_s, ix} -> Enum.at(senators,ix) end)
      if min_val != nil do
        IO.inspect(choosen, label: "Choosen")
      end
    end
    if min_val == nil do
      res.objective
    else
      IO.inspect(res.statistics.solution_count, label: "Number of solutions:")
    end
  end
   
end
