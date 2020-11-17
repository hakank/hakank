#=

  Euler #39 in Julia.

  """
  If p is the perimeter of a right angle triangle with integral length sides,
  {a,b,c}, there are exactly three solutions for p = 120.

  {20,48,52}, {24,45,51}, {30,40,50}

  For which value of p <= 1000, is the number of solutions maximised?
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.11346191s
# This is surprinsingly slow!
function euler39a()
    n = 1000;
    # squares = Set((1:n).|>i->i*i) # slightly slower
    squares = Dict((1:n).|>i->i*i=>1)
    valid = []
    for x in keys(squares)
        for y in keys(squares)
            if x < y && haskey(squares,x+y) && (sqrt(x) + sqrt(x) + sqrt(x+y)) < 1000
                push!(valid,[x, y])
            end
        end
    end

    counts = Dict()
    for x in valid
        c = floor(Int,sqrt(x[1]) + sqrt(x[2]) + sqrt(x[1]+x[2]))
        get!(counts,c,0)
        counts[c] += 1
   end

    # find max count
    maxV = maximum(values(counts))
    t = [i for i in keys(counts) if counts[i] == maxV]
    return t[1]
end

run_euler(euler39a)
