#
# N-queens problem via MiniZinc-Python interface in Julia.
# 
# For more info about this Python package, see:
# - https://minizinc-python.readthedocs.io/en/latest/index.html
# - https://gitlab.com/minizinc/minizinc-python
# 
# The MiniZinc model is a simple n-queens problem.
#
# This program tests both the model as text string and as a MiniZinc file.
# It assumes there is a file "nqueens.mzn" with the following content
# (but it don't crash if there isn't)
#
# """
#        include "alldifferent.mzn";
#        int: n; %  = 8; % The number of queens.
# 
#        array [1..n] of var 1..n: q;
#
#        constraint alldifferent(q);
#        constraint alldifferent(i in 1..n)(q[i] + i);
#        constraint alldifferent(i in 1..n)(q[i] - i);
#        solve :: int_search(q, first_fail, indomain_split) satisfy;
# """
#
# For more MiniZinc models see my MiniZinc page: http://hakank.org/minizinc/
#
#
# Some issues/findings:
#  - the timeout must be a Python datetime.timedelta object 
#  - adding the instance size ("n") must be done explicitly with
#    instance.__setitem__("n", n)
#    For some reason instance["n"] don't work.
#  - depending on the number of solutions, printing the results 
#    must be handled slightly different.
#
# Note that I have not implemented/checked all the bells&whistles
# of the MiniZinc-Python package. 
# This is just a proof of concept program...
# 
#
using PyCall

minizinc = pyimport("minizinc")

# For timeout (which is a timedelta object. See below.)
datetime = pyimport("datetime")
timedelta = datetime.timedelta 

function nqueens(n=8, all_solutions=true,show_stats=true,show_solutions=true;model_file=nothing,timeout=nothing,free_search=false)

    println("queens($n)")
    Solver = minizinc.Solver
    Instance = minizinc.Instance
    Model = minizinc.Model 
    
    solver = nothing
    try 
        # Testing different FlatZinc solvers
        solver = Solver.lookup("gecode")
        # solver = Solver.lookup("picat_sat")
        # solver = Solver.lookup("chuffed")
        # solver = Solver.lookup("jacop")
        # solver = Solver.lookup("izplus")
        # solver = Solver.lookup("optimathsat")
        # solver = Solver.lookup("yuck")
        # solver = Solver.lookup("choco")
        # solver = Solver.lookup("mistral")
        # solver = Solver.lookup("sunnycp")
        # solver = Solver.lookup("or_tools")
        # solver = Solver.lookup("oscar_cbls")
        
        # println("Solver:", solver)
    catch 
        println("Could not find solver!")
        return
    end

    println("solver: ", solver.name)

    model = nothing
    if model_file !== nothing 
        model = Model(model_file) # with a MiniZinc file
    else 
        # Model as text
        model = Model()
        model.add_string(
        """
        include "alldifferent.mzn";
        int: n; %  = 8; % The number of queens.

        array [1..n] of var 1..n: q;

        constraint alldifferent(q);
        constraint alldifferent(i in 1..n)(q[i] + i);
        constraint alldifferent(i in 1..n)(q[i] - i);
        solve :: int_search(q, first_fail, indomain_split) satisfy;
        """
        )
    end 

    instance = Instance(solver, model)
    # instance["n"] = n  # This don't work
    instance.__setitem__("n",n) # This works!

    # Find and print all solution(s)
    result = instance.solve(all_solutions=all_solutions, free_search=free_search,timeout=timeout)

    num_solutions = result.statistics["nSolutions"]
    if num_solutions === nothing 
        num_solutions = length(result)
    end
    println("Number of solutions ", num_solutions )
    println("status: ", result.status)
    if num_solutions > 0 
        if show_solutions
            if all_solutions 
                for i in 1:length(result)
                    println(convert(Array{Int,1},result[i]["q"])) 
                end
            else 
                println(convert(Array{Int,1},result.solution["q"]))
            end
        end
        if show_stats
            println("\nStatistics:")
            for (s,v) in result.statistics
                println(s, ": ", v)
            end
        end
    else 
        println("No solution found!")
    end 
    return num_solutions
end 

result = nqueens(8,true,false,false);

# Run with a MiniZinc model file
model_file = "nqueens.mzn"
if isfile(model_file)
    result = nqueens(8,true,false,false,model_file=model_file)
else 
    println("Cannot find the MiniZinc model $model_file")
end 

# The timeout is a datetime.timeout object!
# 10 seconds timeout
timeout = timedelta(seconds=10)

# My standard sizes ...
# (Note: n=500 is slow, but n=501 is faster...)
for n in [8,12,20,100,200,501,1000]
    println("\nn:$n")
    @time num_solutions = nqueens(n,false,false,true,timeout=timeout,free_search=true);
end 
