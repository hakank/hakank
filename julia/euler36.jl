#=

  Euler #36 in Julia.

  Problem 36:
  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic
  in both bases.

  Find the sum of all numbers, less than one million, which are palindromic
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not
   include leading zeros.)
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.06355465s
function euler36a()
    s = 0
    for n in 1:999999
        if palindromic_number(n) &&
            palindromic_list(digits(n,base=2))
            # palindromic_list(string(n,base=2))
            s += n;
        end
    end
    return s
end

# 0.19043701s
function euler36b()
    return ((1:999999)
        .|>n->n*palindromic_number(n)
        .|>n->n*palindromic_list(digits(n,base=2))
        )|>sum
end

run_euler(euler36a)
# run_euler(euler36b)
