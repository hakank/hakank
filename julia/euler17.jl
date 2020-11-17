#=

  Euler #17 in Julia.

  Problem 17:
  """
  If the numbers 1 to 5 are written out in words: one, two, three, four, five,
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
  words, how many letters would be used?

  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of
  "and" when writing out numbers is in compliance with British usage.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function english(n)
    divs  =      [1_000_000_000, 1_000_000,  1_000,       100]
    divnames  =  ["billion", "million", "thousand", "hundred"]
    prefixes  =  ["0", "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"]
    _ordinals  = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                      "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
                      "fourteenth","fifteenth", "sixteenth", "seventeenth",
                      "eighteenth", "nineteenth"]
    cardinals =  ["one", "two", "three", "four", "five", "six", "seven",
                      "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];

    s = ""
    printed = 0
    if n < 0
        s = "minus" * s;
        n = -n
    end
    d = 0
    for i in 1:length(divs)
        d = floor(Int,n / divs[i])
        n %= divs[i]
        if d != 0
            s *= english(d) * divnames[i];
            printed = 1
        end
    end

    if n > 0 && printed == 1
        s *= "and";
    end
    if n == 0
        # dummy
    elseif n > 19
        d = floor(Int,n / 10)
        n %= 10
        s *= prefixes[d] * "ty" * english(n)
    else
        s *= cardinals[n]
    end

    return s
end

# 0.00138457s
function euler17a()
    return sum((1:1000).|>i->length(english(i)))
end

run_euler(euler17a)
