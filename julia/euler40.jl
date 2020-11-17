#=

  Euler #40 in Julia.

  Problem 40:
  """
  An irrational decimal fraction is created by concatenating the positive integers:

  0.123456789101112131415161718192021...

  It can be seen that the 12th digit of the fractional part is 1.

  If dn represents the nth digit of the fractional part, find the
  value of the following expression.

  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/Julia_progs/

=#

include("Euler.jl")

# 0.00941826s
function euler40a()
    i = 1
    dlen = 1
    p = 1
    index = 10; # Index = 10, 100, 1000, ..., 1000000
    while dlen <= 1000000
        i += 1
        istr = string(i)
        istrlen = length(istr)
        if dlen+istrlen>=index
            p *= parse(Int,istr[index-dlen])
            index *= 10
        end
        dlen += istrlen
    end

    return p
end

run_euler(euler40a)
