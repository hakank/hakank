# 
# Minesweeper in Elixir.
#
# From gecode/examples/minesweeper.cc:
# """
# A specification is a square matrix of characters. Alphanumeric
# characters represent the number of mines adjacent to that field. 
# Dots represent fields with an unknown number of mines adjacent to 
# it (or an actual mine).
# """
#
# E.g.
#      "..2.3."
#      "2....."
#      "..24.3"
#      "1.34.."
#      ".....3"
#      ".3.3.."
# """
#
# Also see:
# * http://www.janko.at/Raetsel/Minesweeper/index.htm
#
# * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
#
# * Ian Stewart on Minesweeper: 
#   http://www.claymath.org/Popular_Lectures/Minesweeper/
#
# * Richard Kaye's Minesweeper Pages
#   http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
#
# * Some Minesweeper Configurations
#   http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
#
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule Minesweeper do
  import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  # alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent  
  alias CPSolver.Model
  # alias CPSolver.Objective
  
  # import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory

  # Problem from Gecode/examples/minesweeper.cc  problem 0
  # 
  # Solution:
  #  1 0 0 0 0 1
  #  0 1 0 1 1 0
  #  0 0 0 0 1 0
  #  0 0 0 0 1 0
  #  0 1 1 1 0 0
  #  1 0 0 0 1 1
  def problem(0) do
    u = -1 # unknown value
    [[u,u,2,u,3,u],
     [2,u,u,u,u,u],
     [u,u,2,4,u,3],
     [1,u,3,4,u,u],
     [u,u,u,u,u,3],
     [u,3,u,3,u,u]]
  end


  #
  # print_instance(p,rows,cols)
  #
  # Pretty print the problem instance `p`.
  # 
  defp print_instance(p,rows,cols) do
    IO.puts("Problem instance:")
    for i <- 0..rows-1 do
      for j <- 0..cols-1 do
         v = mat_at(p,i,j)
         if v >= 0 do
           :io.format("~1w",[v])
         else
           :io.format("~1w",['_'])
         end
      end
      IO.puts("")
    end
    IO.puts("")
  end

  def read_instance(file) do
    {:ok, contents} = File.read(file)
    [_,_ | p ] = contents
                 |> String.split("\n")
                 |> Enum.filter(fn line -> !String.starts_with?(line,"#") end)
                 |> Enum.map(fn s -> String.split(s,"", trim: true)
                     |> Enum.map(fn c -> if c != "." do String.to_integer(c) else -1 end end)
                    end)
    p
  end

  def main(file \\ "") do
    p = if file == "" do problem(0) else read_instance(file) end
    minesweeper(p)    
  end

  # Test all instances in lib/minesweeper*.txt
  def main2() do
    skipping = %{"lib/minesweeper_kaye_splitter.txt" => true}
    
    for file <- Path.wildcard("lib/minesweeper*.txt") do
      IO.puts("Solving file #{file}")
      if Map.has_key?(skipping, file) do
        IO.puts("Skipping this instance since it has too many solutions.\n")
      else
        minesweeper(read_instance(file))
      end
    end
  end

  # Running lib/minesweeper_kaye_splitter.txt
  # which has a huge number of solutions.
  def main3() do
        minesweeper(read_instance("lib/minesweeper_kaye_splitter.txt"))
  end
  
  @doc """
  minesweeper(p) 

  Solves the Minesweeper instance `p`.

  """
  def minesweeper(p) do
    rows = length(p)
    cols = length(Enum.at(p,0))
    print_instance(p,rows,cols)
    
    x = for i <- 0..rows-1 do
          for j <- 0..cols-1 do
            v = CPUtils.mat_at(p,i,j)
            if v >= 0 do
              IntVariable.new(0, name: "x[#{i},#{j}]")
            else
              IntVariable.new(0..1, name: "x[#{i},#{j}]")
            end
          end
        end

    constraints =
     for i <- 0..(rows - 1), j <- 0..(cols - 1), v = mat_at(p, i, j), v >= 0 do
       Sum.new(
         v,
         for a <- -1..1, b <- -1..1,
         (i + a) in 0..(rows - 1), (j + b) in 0..(cols - 1) do
           mat_at(x, i + a, j + b)
         end
       )
     end
   
    model = Model.new(x |> List.flatten,
                      constraints )
    
    Logger.configure(level: :info)

    opts = [
      search: {:first_fail, :indomain_min},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
      ]      
    {:ok, res} = CPSolver.solve_sync(model,
                                     opts
                                     )
    IO.inspect(res.statistics)
    res.solutions
    |> Enum.map(fn s -> Enum.take(s,rows*cols) |> CPUtils.print_matrix(rows,cols) end)

    nil
  end
  
end
