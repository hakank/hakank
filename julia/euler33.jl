#=

  Euler #33 in Julia.

  Problem 33:
  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct,
  is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than
  one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find
  the value of the denominator.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.00000079s
function euler33a()
    s = 1;
    for y in 1:9
        for z in 1:9
            x = 9.0*y*z/(10.0*y-z)
            if floor(x)==x && y/z < 1.0 && x < 10.0
                s = (s*y)/z;
            end
        end
    end

    return 1/s;
end

run_euler(euler33a)
