#=
  xkcd problem (Knapsack)  in Google CP Solver.

  This is a of of my Pythons Or-tools program 
  http://hakank.org/ortools/xkcd.py to Julia 

  Orginal comment (sligly edited):
  """
  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total
  of exact 15.05

  Compare with the following models:
  * Comet: http://www.hakank.org/comet/xkcd.co
  * ECLiPSE: http://www.hakank.org/eclipse/xkcd.ecl
  * Gecode: http://www.hakank.org/gecode/xkcd.cpp
  * Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb
  * MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn
  * Tailor: http://www.hakank.org/minizinc/xkcd.mzn
  * SICtus: http://www.hakank.org/sicstus/xkcd.pl
  * Zinc: http://www.hakank.org/minizinc/xkcd.zinc

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google Or-tools models:
  http://www.hakank.org/or_tools/
  """

#
# Things to change from the Python code 
#  - the import
#  - range -> 1:...  or 0:...-1   (or using Julia's range)
#  - print -> println + adding spaces
#  - format codes: (x%i) -> " x$i"
#  - remove ":" in "if:", "else:" etc  
#  and 
#  - be careful with indentation when copying code otherwise
#    it can be hard to see what it mean...
#
=#
using PyCall

# Be sure that ortools are installed correctly
ot = pyimport("ortools.constraint_solver.pywrapcp")

# Create the solver
solver = ot.Solver("xkcd")
ot.Solver("xkcd")

#
# data
#
num_prices = 6
println("num_prices: $num_prices")

# for price and total: multiplied by 100 to be able to use integers
price = [215, 275, 335, 355, 420, 580]
println("price:$price")
total = 1505
println("total:$total")
products = [
    "mixed fruit", "french fries", "side salad", "host wings",
    "mozzarella sticks", "samples place"
]
println("products:$products")

#
# declare the variables
#

# how many items of each dish
x = [solver.IntVar(0, 10, "x$i") for i in 1:num_prices]
# println("x:$x")
z = solver.IntVar(0, 1505, "z")
# println("z:$z")
#
# constraints
#
solver.Add(z == solver.Sum([x[i] * price[i] for i in 1:num_prices]))
solver.Add(z == total)

#
# solution and search
#
solution = solver.Assignment()
solution.Add([x[i] for i in 1:num_prices])
solution.Add(z)

# println("solution:$solution")

collector = solver.AllSolutionCollector(solution)
# collector = solver.FirstSolutionCollector(solution)
# search_log = solver.SearchLog(100, x[0])
solver.Solve(
    solver.Phase([x[i] for i in 1:num_prices], solver.INT_VAR_SIMPLE,
                solver.ASSIGN_MIN_VALUE), [collector])

num_solutions = collector.SolutionCount()
println("\nnum_solutions: ", num_solutions)
if num_solutions > 0
    # NOTE: The solution ids are 0-based
    # (Julia migh crash if this is wrong)
    for s in 0:num_solutions-1
        println("solution #$(s+1)")
        println("z:", collector.Value(s, z) / 100.0) 
        # But this is 1-based since it's Julia now
        xval = [collector.Value(s, x[i]) for i in 1:num_prices]
        println("x:", xval)
        for i in 1:num_prices
             if xval[i] > 0
                 println(xval[i], " of ", products[i], ": ", price[i] / 100.0)
             end
        end
        println()
    end
    println("num_solutions:", num_solutions)
    println("failures:", solver.Failures())
    println("branches:", solver.Branches())
    println("WallTime:", solver.WallTime())

else
    print("No solutions found")
end
