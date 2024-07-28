# 
# Survo puzzle in Elixir.
#
# http://en.wikipedia.org/wiki/Survo_Puzzle
# """
# Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
# by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
# Survo system which is a general environment for statistical computing and 
# related areas.

# In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
# that each of these numbers appears only once and their row and column sums are 
# equal to integers given on the bottom and the right side of the table. 
# Often some of the integers are given readily in the table in order to 
# guarantee uniqueness of the solution and/or for making the task easier.
# """
#
# See also
# http://www.survo.fi/english/index.html
# http://www.survo.fi/puzzles/index.html
#
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule SurvoPuzzle do
  
  # import Enum # Conflicts with CPSolver.Constraint.Factory.sum
  # import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent  
  alias CPSolver.Model
  
  # import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory


  def print_solution(x,rows, cols,rowsums,colsums) do
    for i <- 0..rows-1 do
      for j <- 0..cols-1 do
        :io.format("~3w",[Enum.at(x,i*cols+j)])
      end
      :io.format(" = ~3w ~n",[Enum.at(rowsums,i)])
    end
    for j <- 0..cols-1 do
      :io.format("~3w",[Enum.at(colsums,j)])
    end
    :io.format("~n~n",[])
  end  

  #
  # From http://en.wikipedia.org/wiki/Survo_Puzzle, first example
  #
  # Solutions should be
  # 
  # 12  6  2 10  = 30
  #  8  1  5  4  = 18
  #  7  9  3 11  = 30
  # 27 16 10 25
  #
  # I.e 12  6  2 10  8  1  5  4  7  9  3 11
  #
  def puzzle(1) do
    rowsums = [30,18,30]
    colsums = [27,16,10,25]
    # 0 is unknown -> to be decided
    problem = [[0, 6, 0, 0],  # 0 means unknown
               [8, 0, 0, 0],
               [0, 0, 3, 0]]
    [rowsums,colsums,problem]
  end

  #
  # From http://en.wikipedia.org/wiki/Survo_Puzzle, second example
  # difficulty 0

  def puzzle(2) do
    rowsums = [9, 12]
    colsums = [9, 7, 5]
    problem = [[0, 0, 3],
               [0, 6, 0]]
    [rowsums,colsums,problem]
  end

  # http://en.wikipedia.org/wiki/Survo_Puzzle, third example
  # difficulty 150 ("open puzzle", i.e. no hints}
  # It's an unique solution.
  # (817 propagations with Gecode/fz, and 33 failures, 88 commits}
  # r = 3;
  # c = 4;
  # rowsums = [24,15,39];
  # colsums = [21,10,18,29];
  # matrix = array2d(1..r, 1..c, 
  #   [
  #     0, 0, 0, 0,
  #     0, 0, 0, 0,
  #     0, 0, 0, 0
  #   ]};
  # Note: this version has no hints
  def puzzle(3) do
    rowsums = [24,15,39]
    colsums = [21,10,18,29]
    problem = [[0, 0, 0, 0],
               [0, 0, 0, 0],
               [0, 0, 0, 0]]
    [rowsums,colsums,problem]
  end



  # same as above but with hints: difficulty 0
  # (15 propagations with Gecode/fz, no failures, no commits]
  # matrix = array2d(1..r, 1..c, 
  #    [
  #      7, 0, 5, 0,
  #      0, 1, 0, 8,
  #      0, 0, 11, 0
  #    ]];
  def puzzle(4) do
    rowsums = [24,15,39]
    colsums = [21,10,18,29]
    problem = [[7, 0, 5, 0],
               [0, 1, 0, 8],
               [0, 0, 11, 0]]
    [rowsums,colsums,problem]    
  end



  # http://www.survo.fi/puzzles/280708.txt, third puzzle
  # Survo puzzle 128/2008 (1700] #364-35846
  #
  #    A  B  C  D  E  F
  # 1  *  *  *  *  *  * 30
  # 2  *  * 18  *  *  * 86
  # 3  *  *  *  *  *  * 55
  #   22 11 42 32 27 37
  #
  # Solution:
  #  4  1 10  5  3  7 =  30 
  # 12  8 18 16 15 17 =  86 
  #  6  2 14 11  9 13 =  55 
  # 22 11 42 32 27 37
  # 
  def puzzle(5) do
    rowsums = [30, 86, 55]
    colsums = [22, 11, 42, 32, 27, 37]
    problem = [[0, 0,  0, 0, 0, 0],
               [0, 0, 18, 0, 0, 0],
               [0, 0,  0, 0, 0, 0]]
    [rowsums,colsums,problem]
  end

  #
  # http://en.wikipedia.org/wiki/Survo_Puzzle, under "Swapping method"
  # (open puzzle]
  #
  # Solution:
  # 15 16 12  8 =  51 
  # 14 11  7  4 =  36 
  # 13 10  6  3 =  32 
  #  9  5  1  2 =  17 
  # 51 42 26 17

  def puzzle(6) do
    rowsums = [51,36,32,17]
    colsums = [51,42,26,17]
    problem = [[0, 0, 0, 0],
               [0, 0, 0, 0],
               [0, 0, 0, 0],
               [0, 0, 0, 0]]
    [rowsums,colsums,problem]    
  end

    
  def main() do

    for p <- 1..6 do
      IO.puts("\nPuzzle ##{p}")
      [rowsums,colsums,problem] = puzzle(p)
      survo_puzzle(rowsums,colsums,problem)
    end
  end

  def survo_puzzle(rowsums,colsums,problem) do
    rows = length(rowsums)
    cols = length(colsums)
    dom = 1..rows*cols

    # Decision variables
    x = for i <- 0..rows-1 do
          for j <- 0..cols-1 do
             v = CPUtils.mat_at(problem,i,j)
             if v > 0 do
               # > 0: this is a hint
               IntVariable.new(v, name: "x[#{i},#{j}]")
             else
               # 0: unknown
               IntVariable.new(dom, name: "x[#{i},#{j}]")
             end
          end
        end

    x_flatten = x |> List.flatten

    #
    # Constraints
    #
    
    all_different_constraint = AllDifferent.new(x_flatten)
    
    # Row constraints
    row_constraints = for {s,row} <- Enum.zip(rowsums,x) do
      Sum.new(s,row)
    end

    # Column constraints
    col_constraints = for {s,col} <- Enum.zip(colsums, CPUtils.transpose(x)) do
      Sum.new(s,col)
    end

    constraints = [all_different_constraint] ++ row_constraints ++ col_constraints
    
    model = Model.new(x_flatten,
                      constraints
    )

    Logger.configure(level: :info)    
    {:ok, result} =
      CPSolver.solve_sync(model,
        search: {:first_fail, :indomain_min},
        # stop_on: {:max_solutions, 1},
        timeout: :infinity,
        space_threads: 12
      )

    # IO.inspect(result.solutions)
    # IO.inspect(Enum.map(result.solutions, fn sol -> Enum.zip(result.variables, sol) end))    
    IO.inspect(result.statistics)

    result.solutions
    |> Enum.map(fn s -> s |> Enum.take(rows*cols) end)
    |> Enum.map(fn s -> print_solution(s,rows,cols,rowsums,colsums) end)

    # result.statistics
    
  end
  
end
