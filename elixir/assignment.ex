# 
# Assignment problems in Elixir.
#
  # Different assignments problem, both minimization and maximization. 
  # See the sources of the problem below.
 
  # Compare to the following MiniZinc models, from which these problems
  # are taken:
  # * http://www.hakank.org/minizinc/assignment.mzn
  # * http://www.hakank.org/minizinc/assignment2.mzn
  # * http://www.hakank.org/minizinc/assignment2_2.mzn 
  # * http://www.hakank.org/minizinc/assignment3.mzn
  # * http://www.hakank.org/minizinc/assignment5.mzn
  # * http://www.hakank.org/minizinc/assignment6.mzn

# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule Assignment do
  
  import CPUtils
  
  alias CPSolver.IntVariable
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  alias CPSolver.Constraint.Sum  
  alias CPSolver.Constraint.LessOrEqual
  # alias CPSolver.Constraint.NotEqual
  # alias CPSolver.Constraint.Equal    
  alias CPSolver.Model
  alias CPSolver.Objective
  
  import CPSolver.Constraint.Factory
  import CPSolver.Variable.View.Factory

  #
  # Data from 
  # Winston "Operations Research", Assignment Problems, page 393f
  # I added the fifth column
  #
  def problem(1) do
    op = :minimize
    cost = [[14,  5, 8,  7, 15],
            [ 2, 12, 6,  5,  3],
            [ 7,  8, 3,  9,  7],
            [ 2,  4, 6, 10,  1]]
    [op,cost]
  end

  # 
  # Winston "Operations Research", page 398, swimming team example
  # (original version]
  # See http://www.hakank.org/minizinc/assignment2.mzn 
  # 
  def problem(2) do
    op = :minimize
    cost = [[54, 54, 51, 53], 
            [51, 57, 52, 52],
            [50, 53, 54, 56],
            [56, 54, 55, 53]]
    [op,cost]
  end

  # 
  # Winston "Operations Research", page 398, swimming team example
  # See http://www.hakank.org/minizinc/assignment2_2.mzn 
  # expanded version
  #
  def problem(3) do
    op = :minimize
    cost = [[54, 54, 51, 53,   50,60,70,80,90,100], 
            [51, 57, 52, 52,   40,50,60,70,80, 90],
            [50, 53, 54, 56,   40,50,60,80,93, 69],
            [56, 54, 55, 53,   60,80,40,60,50,100]]
    [op,cost]
  end


  #
  # Winston "Operations Research", page 399
  # 
  # """
  # Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
  # are marooned on a desert island with Jennifer Anniston,
  # Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
  # The 'compatibility matrix' in Table 52 indicate how much happiness
  # each couple would experience if the spend all their time toghether.
  # The happiness earned by a couple is proportional to the fraction 
  # of time the spend toghether. 
  # ...
  # The optimal solution requires that that each person send all their
  # time with one person of the opposite sex, so this result is often
  # referred to as the Marriage Theorem.
  # """
  #
  # See http://www.hakank.org/minizinc/assignment3.mzn

  # males:
  # 1 "Tom Cruise"
  # 2 "Freddie Prinz Jr"
  # 3 "Harrison Ford"
  # 4 "Mark LeBlanc"
  #
  # females:
  # 1 "Jennifer Anniston"
  # 2 "Courtney Cox"
  # 3 "Gwynneth Paltrow"
  # 4 "Julia Roberts"
  def problem(4) do
    op = :maximize
    cost = [[7, 5, 8, 2],
            [7, 8, 9, 4],
            [3, 5, 7, 9],
            [5, 5, 6, 7]]
    [op,cost]
  end


  # From
  #  "SAS OR 9.1 User's Guide Mathematical Programming"
  # """
  # Consider assigning five programmers to five programming jobs. Each
  # programmer prefers specific programming job over others. [...] 
  # Suppose you ask each programmer to rank the jobs according to preference
  # (using 1 for the most preferred job and 5 for the least preffered job].
  # PROC ASSIGN maximizes the total preference of the group by minimizing the
  # sum of the preferences. 
  # 
  #    PROGRAMMER     JOB1 JOB2 JOB3 JOB4 JOB5
  #    PROGRAMMER1    4    1    3    5    2
  #              2    2    1    3    4    5
  #              3    3    2    4    1    5
  #              4    2    3    4    5    1
  #              5    4    2    3    1    5
  # 
  # """
  # 
  # See http://www.hakank.org/minizinc/assignment5.mzn
  # 
  def problem(5) do
    op = :minimize
    cost = [[4, 1, 3, 5, 2],
            [2, 1, 3, 4, 5],
            [3, 2, 4, 1, 5],
            [2, 3, 4, 5, 1],
            [4, 2, 3, 1, 5]]
    [op,cost]
  end


  #
  # From GLPK:s example assign.mod:
  # """
  # The assignment problem is one of the fundamental combinatorial
  # optimization problems.
  #
  # In its most general form, the problem is as follows:
  #
  # There are a number of agents and a number of tasks. Any agent can be
  # assigned to perform any task, incurring some cost that may vary
  # depending on the agent-task assignment. It is required to perform all
  # tasks by assigning exactly one agent to each task in such a way that
  # the total cost of the assignment is minimized.
  #
  # (From Wikipedia, the free encyclopedia.] 
  # """
  # 
  # """
  # These data correspond to an example from [Christofides].
  # """
  #
  # See http://www.hakank.org/minizinc/assignment6.mzn
  #
  def problem(6) do
    op = :minimize
    cost = [[13, 21, 20, 12,  8, 26, 22, 11],
            [12, 36, 25, 41, 40, 11,  4,  8],
            [35, 32, 13, 36, 26, 21, 13, 37],
            [34, 54,  7,  8, 12, 22, 11, 40],
            [21,  6, 45, 18, 24, 34, 12, 48],
            [42, 19, 39, 15, 14, 16, 28, 46],
            [16, 34, 38,  3, 34, 40, 22, 24],
            [26, 20,  5, 17, 45, 31, 37, 43]]
    [op,cost]
  end


  
  def main() do

    for p <- 1..6 do
      IO.puts("\nProblem #{p}")
      [op,cost] = problem(p)
      assignment(op, cost)
    end
    
  end

  def assignment(op, cost) do
    
    rows = length(cost)
    cols = length(Enum.at(cost,0))

    # Who to assign to what task
    x = for i <- 0..rows-1 do
           for j <- 0..cols-1 do
              IntVariable.new(0..1, name: "x[#{i},#{j}]")
           end
    end

    x_flatten = x |> List.flatten
    
    total_cost = IntVariable.new(0..Enum.sum(cost |> List.flatten), name: "total_cost") 

    
    # exacly one assignment per row, all rows must be assigned
    row_constraints = for i <- 0..rows-1  do
                        Sum.new(1,for j <- 0..cols-1 do CPUtils.mat_at(x,i,j) end)
    end
    
    # zero or one assignments per column
    column_constraints = for j <- 0..cols-1  do
                                           {c_var,c_cons} = sum(for i <- 0..rows-1 do mat_at(x,i,j) end)
                                           less_cons = LessOrEqual.new(c_var,1)
                                           [c_cons,less_cons]
    end

    # calculate total_cost
    total_cost_constraint = Sum.new(total_cost, for i <- 0..rows-1, j <- 0..cols-1 do
                                                    mul(mat_at(x,i,j),mat_at(cost,i,j))
                                                end)

    model = Model.new(x_flatten ++ [total_cost],
      (row_constraints ++ column_constraints ++ [total_cost_constraint]) |> List.flatten,
      objective: if op == :minimize do Objective.minimize(total_cost) else Objective.maximize(total_cost) end
    )
    
    Logger.configure(level: :info)

    opts = [
      search: {:first_fail, :indomain_min},
      #search: {:input_order, :indomain_min},      
      # search: {:first_fail, :indomain_random},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
      ]      
    {:ok, res} = CPSolver.solve_sync(model,
                                     opts
                                     )
    # IO.inspect(res.statistics)
    sol = res.solutions |> List.last

    total_cost_val = Enum.at(sol,rows*cols)
    IO.puts("total_cost: #{total_cost_val}")
    mat = Enum.take(sol,rows*cols)
    print_matrix(mat,rows,cols)

    for i <- 0..rows-1, j <- 0..cols-1, Enum.at(mat,i*cols+j) == 1 do j end
    |> IO.inspect(label: "assigned")

    
  end
  
end
