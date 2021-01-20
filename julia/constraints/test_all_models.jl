#=
    Test all my constraint models in ConstraintSolver.jl 

    This program test all

    Program created by Hakan Kjellerstrand, hakank@gmail.com
    See also my Julia page: http://www.hakank.org/julia/

=#
using ConstraintSolver, JuMP
using Cbc, GLPK, Ipopt
using HTTP

# Skip testing these files
skip = Dict(
    "test_all_models.jl" => true, # Well, we run it just one, right. :-) 
    "constraints_utils.jl" => true,
    "minesweeper_problems.jl" => true,
    "sudoku_problems.jl" => true,
    "survo_puzzle_problems.jl" => true,
    "quasigroup_completion_problems.jl" => true,
    "assignment_instances.jl" => true,
    
)

#
# Read all the public models from my http://hakank.org/julia/constraints/index.html
#
function read_from_hakank_org()
    url = "http://hakank.org/julia/constraints/index.html"
    res = HTTP.get(url)
    body = String(res.body)
    regex = r"<a href=\"([^\s]+?.(?:jl))\""sm
    all = []
    for t in [t[1] for t in eachmatch(regex,body)]
        if get(skip,t,false)
            println("Skipping $t")
            continue
        end
        push!(all, t)
    end
    return all
end

# Test all my public ConstraintSolver.jl models
function test_all() 
    
    files = read_from_hakank_org()

    total_time = 0
    for file in files 
        println("\nRunning $file")
        @time include(file)
    
    end
    
end

test_all()
# read_from_hakank_org()