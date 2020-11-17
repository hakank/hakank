#=

  Euler #4 in Julia.

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """


  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#
include("Euler.jl")

# 0.00958293s
function euler4a()
    max = 0
    from = 100
    to = 999
    for i in from:to, j in i:to
        ij = i*j
        if ij > max && palindromic_number(ij)
            max = ij
        end
    end
    return max;
end

# 0.313482985s
function euler4b()
    from = 100
    to = 999
    a = []

    # Note: It seems that one cannot assign a variable (ij=i*j)
    # in a function pipe
    (from:to).|> i->
        (i:to).|> j->
            # ij = i * j; # this don't work! (Why?)
            if palindromic_number(i*j)
                push!(a,i*j)
            end
    return maximum(a)
end

# 0.085608391s
function euler4c()
    from = 100
    to = 999
    # vcat(a...) flattens the array
    a = (from:to).|>i->(i:to).|>j->i*j
    return maximum(filter(ij->palindromic_number(ij),vcat(a...)))
end

run_euler(euler4a)
# run_euler(euler4b)
# run_euler(euler4c)
