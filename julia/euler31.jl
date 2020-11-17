#=

  Euler #31 in Julia.

  Problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")


# DP approach
function coins(c,money,m)
    len = length(c)
    if m == len
        return 1
    end

    sum1 = 0
    for i in m:len
        if money - c[i] == 0
            sum1 += 1
        end
        if  money - c[i] > 0
            sum1 += coins(c, money-c[i], i)
        end
    end
    return sum1;
end

# 0.00058022s
function euler31a()
    c = [200,100,50,20,10,5,2,1]
    return coins(c, 200, 1)
end

run_euler(euler31a)
