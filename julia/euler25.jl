#=

  Euler #25 in Julia.

  Problem 25:
  """
  The Fibonacci sequence is defined by the recurrence relation:

     Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.

  Hence the first 12 terms will be:

     F1 = 1
     F2 = 1
     F3 = 2
     F4 = 3
     F5 = 5
     F6 = 8
     F7 = 13
     F8 = 21
     F9 = 34
     F10 = 55
     F11 = 89
     F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?")
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using Memoization
include("Euler.jl")


# Is not faster
@memoize function fib_len_memo(n)
    return fib(BigInt(n))|>string|>length
end

function fib_len(n)
    return fibmemBigInt(n)|>string|>length
end


# Brute force: 0.09872957s
function euler25a()
    i = 0
    len = 0
    while len < 1000
        i += 1
        len = fibmemBigInt(i)|>string|>length
    end

    return i;
end

#
# Using some heuristics to find the upper limit
# (from my Picat solution).
# 0.08765257s
function euler25b()
    target = 1000;
    foundUpper = 0;
    i = 1;
    fibLen = 0;
    step = 43;
    # Get the upper limit
    while fibLen < target && foundUpper == 0
        fibLen = fib_len(step*i);
        if fibLen > target
            foundUpper = i
            break
        end
        i += 1
    end

    # Now check all numbers from Step*(FoundUpper-1) .. Step*FoundUpper
    # The target must be in that interval.
    f = step*(foundUpper-1)
    fibLen = fib_len(f);
    while fibLen < target && f <= step*foundUpper
        f += 1
        fibLen = fib_len(f)
    end

    return f
end

# run_euler(euler25a)
run_euler(euler25b)
