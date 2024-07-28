#
# 
#
defmodule CPUtils do

  # alias CPSolver.IntVariable
  alias CPSolver.Constraint.Sum
  # alias CPSolver.Constraint.Equal
  alias CPSolver.Constraint.LessOrEqual
  alias CPSolver.Constraint.AllDifferent.FWC, as: AllDifferent  
  # alias CPSolver.Model
  import CPSolver.Variable.View.Factory
  
  #
  # "System" / General functions.
  #

  @doc """
  timeit(fun) 

  A simple timing function, returns the time in seconds (as a string).

  ##Examples##

       > Util.timeit(&Test1.main/0)

       > Util.timeit(fn () -> Test1.main() end)

  """
  def timeit(fun) do
    {time0, res} = :timer.tc(fun, [])
    IO.inspect(res)
    (time0 / 1_000_000) |> :erlang.float_to_binary([:compact, decimals: 5]) 
  end

  @doc """
  mat_at(m,i,j)

  Returns the value (`i`,`j`) of the 2d matrix `mat`.

  ##Examples##

       iex> [[1,2,3],[4,5,6],[7,8,9]] |> mat_at(1,2)
       6
  """
  def mat_at(m,i,j) do
    m |> Enum.at(i) |> Enum.at(j)
  end


  @doc """
  transpose(m) 

  Returns a transposed version of `m`.

  ##Example##

       iex> [[1,2,3],[4,5,6],[7,8,9]] |> transpose
       [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  """
  def transpose(m) do
    Enum.zip_with(m, &Function.identity/1)
  end



  @doc """
  print_matrix(x,rows,cols, format \\ "~2w")

  Pretty print `x` as a matrix.
  Note: `x` is assumed to be a list of rows*cols elements.
  `format` is the spacing of the values, defaults to "~2w".

  """
  def print_matrix(x,rows,cols, format \\ "~2w") do  
    for i <- 0..rows-1 do
      for j <- 0..cols-1 do
        :io.format(format,[Enum.at(x,i*cols+j)]) 
      end
       IO.puts("")
    end
    IO.puts("\n")
  end
  
  #
  # Decomposition of constraints
  #

  @doc """
  latin_square(x) 

  Ensures that an n x n matrix `x` is a Latin Square.

  ##Examples##

      > latin_square(x)

  """
  def latin_square(x) do
    row_constraints = Enum.map(x,fn row -> AllDifferent.new(row) end)
    col_constraints = Enum.map(x |> transpose, fn row -> AllDifferent.new(row) end)
    row_constraints ++ col_constraints
  end

  @doc """
  scalar_product(xs,ys,total)

  Ensures that `xs` * `ys` = `total`. 
  It is assumed that `xs` and `ys` are of the same length.
  `total` can be either a decision variable or a constraint.
  """
  def scalar_product(xs,ys,total) do
    [Sum.new(total, for {xi,yi} <- Enum.zip(xs,ys) do mul(xi,yi) end )]
  end


  @doc """
  increasing(x)

  Decomposition of the global constraint `increasing` which ensures that
  `x` is in increasing order.

  """
  def increasing(x) do
    len = length(x)
    for i <- 1..len-1 do LessOrEqual.new(Enum.at(x,i-1),Enum.at(x,i)) end
  end
  
  @doc """
  decreasing(x)

  Decomposition of the global constraint `increasing` which ensures that
  `x` is in increasing order.

  """
  def decreasing(x) do
    len = length(x)
    for i <- 1..len-1 do LessOrEqual.new(Enum.at(x,i),Enum.at(x,i-1)) end
  end
  
  @doc """
  get_solution_value(res,sol,var_name)

  Return the solution of variable named `var_name` (e.g. `"x[3]"`) for
  a specific solution `sol`.
  Note: `var_name` handles only the single named variables found in 
        `res.variables`.
  """
  #
  # For some reason I cannot add these examples in the @doc section since
  # it gives an error that get_solution_values/3 does not exist.
  # 
  # Examples:
  # x1_val = xxxget_solution_values(res,sol,"x[1]")
  # x_val = for i <- 0..n-1, do: get_solution_values(res,sol,"x[#{i}]")
  #
  def get_solution_value(res,sol,var_name) do
    var_idx = Enum.find_index(res.variables, fn v -> v == var_name end)
    var_idx && Enum.at(sol, var_idx) 
  end
  
end
