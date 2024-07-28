#
# All interval problem in Elixir.
#
# CSPLib problem number 7
# http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
# """
# Given the twelve standard pitch-classes (c, c , d, ...), represented by
# numbers 0,1,...,11, find a series in which each pitch-class occurs exactly
# once and in which the musical intervals between neighbouring notes cover
# the full set of intervals from the minor second (1 semitone) to the major
# seventh (11 semitones). That is, for each of the intervals, there is a
# pair of neigbhouring pitch-classes in the series, between which this
# interval appears. The problem of finding such a series can be easily
# formulated as an instance of a more general arithmetic problem on Z_n,
# the set of integer residues modulo n. Given n in N, find a vector
# s = (s_1, ..., s_n), such that (i) s is a permutation of
# Z_n = {0,1,...,n-1}; and (ii) the interval vector
# v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of
# Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is
# called an all-interval series of size n; the problem of finding such
# a series is the all-interval series problem of size n. We may also be
# interested in finding all possible series of a given size.
# """
#
# Note: This version is based on a model created by Boris Okner.
# 
# Result for n=8:
# x: [4,1,8,2,7,3,5,6] diffs: [3,7,6,5,4,2,1]
# x: [5,4,2,6,3,8,1,7] diffs: [1,2,4,3,5,7,6]
# x: [2,8,1,6,5,3,7,4] diffs: [6,7,5,1,2,4,3]
# x: [5,2,8,1,6,4,3,7] diffs: [3,6,7,5,2,1,4]
# x: [3,4,6,2,7,1,8,5] diffs: [1,2,4,5,6,7,3]
# x: [2,8,1,6,3,7,5,4] diffs: [6,7,5,3,4,2,1]
# x: [5,4,2,8,1,6,3,7] diffs: [1,2,6,7,5,3,4]
# x: [4,3,8,1,7,5,2,6] diffs: [1,5,7,6,2,3,4]
# x: [5,2,6,4,3,8,1,7] diffs: [3,4,2,1,5,7,6]
# x: [2,7,1,8,4,5,3,6] diffs: [5,6,7,4,1,2,3]
# x: [3,7,2,8,1,4,6,5] diffs: [4,5,6,7,3,2,1]
# x: [4,5,3,6,2,7,1,8] diffs: [1,2,3,4,5,6,7]
# x: [3,8,1,7,4,2,6,5] diffs: [5,7,6,3,2,4,1]
# x: [4,3,7,5,2,8,1,6] diffs: [1,4,2,3,6,7,5]
# x: [4,3,5,8,1,7,2,6] diffs: [1,2,3,7,6,5,4]
#
#
# Comparison with Picat and MiniZinc/Gecode (in seconds) for all solutions:
# 
#  n   Fixpoint  Picat    Gecode
# -----------------------------
#  8     0.5s    0.005s   0.003s
#  9     2.4s    0.024s   0.008s
# 10    13.7s    0.048s   0.034s
# 11    79.3s    0.297s   0.184s
# 12   469.3s    1.584s   0.922s
# 13  2982.2s    9.462s   5.178s
# 
#
# This program was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my Elixir page: http://www.hakank.org/elxir/
#
defmodule AllInterval do

  import CPUtils
  
  alias CPSolver.IntVariable
  alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent
  # alias CPSolver.Constraint.Sum
  alias CPSolver.Constraint.LessOrEqual
  alias CPSolver.Constraint.Absolute
  # alias CPSolver.Constraint.Equal
  alias CPSolver.Model
  # alias CPSolver.Objective

  import CPSolver.Constraint.Factory
  # import CPSolver.Variable.View.Factory

  def main() do
    n = 8

    x =
      for i <- 0..(n - 1) do
        IntVariable.new(1..n, name: "x[#{i}]")
      end

    diffs =
      for i <- 0..(n - 2) do
        IntVariable.new(1..(n - 1), name: "diffs[#{i}]")
      end

    constraints =
      for k <- 0..(n - 2) do
        {difference_var, difference_constraint} = subtract(Enum.at(x, k + 1), Enum.at(x, k))
         [Absolute.new(difference_var, Enum.at(diffs, k)), ## |difference_var| = diffs[k]
          difference_constraint]
      end
      |> List.flatten()

    model =
      Model.new(
        x ++ diffs,
        constraints ++
          [
            AllDifferent.new(x),
            AllDifferent.new(diffs),
            # symmetry breaking
            LessOrEqual.new(Enum.at(x, 0), Enum.at(x, n - 1)),
            # symmetry breaking
            LessOrEqual.new(Enum.at(diffs, 0), Enum.at(diffs, 1))
          ]
      )

    Logger.configure(level: :info)

    opts = [
      # search: {:first_fail, :indomain_min},
      # search: {:first_fail, :indomain_max},      
      search: {:input_order, :indomain_min},
      # search: {:input_order, :indomain_random},
      space_threads: 12,
      timeout: :infinity,
      # stop_on: {:max_solutions, 2},
    ]

    {:ok, res} =
      CPSolver.solve_sync(
        model,
        opts
      )

    res.solutions
    |> Enum.map(fn sol ->
      x_vals = for i <- 0..n-1, do: get_solution_value(res,sol,"x[#{i}]")
      diffs_vals = for i <- 0..n-2, do: get_solution_value(res,sol,"diffs[#{i}]")
      :io.format("x: ~w diffs: ~w~n", [x_vals, diffs_vals])
    end)
    IO.inspect(res.statistics)
  end
end
