#=

  Run the Project Euler programs (# 1:50) in Julia.

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
using Distributed
include("Euler.jl")
total = 0
@time @sync @distributed for p in 1:50
    global total
    s = "euler$p.jl"
    if isfile(s)
        # include(s)
        t = @timed include(s)
        total += t.time
    end
end
println("Run total: $(total)s")
