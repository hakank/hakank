#=
  Euler #1 in Julia.

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00001165s
function euler1a()
  sum([i*(i%3==0||i%5==0) for i=1:999])
end

# 0.00001533s
function euler1b()
  sum([i for i in 1:999 if i%3==0||i%5==0])
end

# 0.00000276s
function euler1c()
   s = 0
   for i in 1:999
       if i % 3 == 0 || i % 5 == 0
           s+=i
       end
   end
   s
end

# 0.00001128s
function euler1d()
    filter(x-> x % 3 == 0 || x % 5 == 0,1:999)|>sum
end

# 0.00000898s
function euler1e()
    (1:999).|>(x-> x*(x % 3 == 0 || x % 5 == 0))|>sum
end


# run_euler(euler1a)
# run_euler(euler1b)
run_euler(euler1c)
# run_euler(euler1d)
# run_euler(euler1e)
