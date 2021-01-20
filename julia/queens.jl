#=
N-queens problem in Julia. 

Cf with the ConstraintSolver.jl solution: http://hakank.org/julia/constraint/nqueens.jl

This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
See also my Julia page: http://www.hakank.org/julia/

=#

include("jl_utils.jl") # for next_permutation 





# Brute force version using next_permutation
# Find all solutions
function queens(n=8,num=0,printit=true) 
    println("n:$n")
    q = collect(1:n)
    rev = reverse(q)
    c = 0
    while q != rev
      check = true
      for i in 1:n
        for j in 1:i-1
          if q[i] === q[j] ||
            q[i] + i === q[j] + j ||
            q[i] - i === q[j] - j
            check = false
            break
          end
        end
      end
    
      if check === true
        c += 1 
        if printit
          println(q)
        end
        if num > 0 && c >= num 
          return c
        end
      end
      if q != rev 
        q = next_permutation(q)
      end
    end
    if printit
      println(c)
    end
  
    return c
end

@time queens(8,0, true)

@time queens(8,0, false)

println("\nCount all solutions:")
for n in 2:10
  @time c = queens(n,0,false)
  println("n:$n count:$c")
end

println("\nFirst solution:")
for n in 2:12
  @time c = queens(n,1,true)
  println("n:$n count:$c")
end
