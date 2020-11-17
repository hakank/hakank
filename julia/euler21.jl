#=
  Euler #21 in Julia.

  Problem 21
  """
  Let d(n) be defined as the sum of proper divisors of n (numbers less
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable
  pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are
  1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
  The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# 0.12387843s
function euler21a()
    s = Dict()
    for a in 1:9999
        b = sum(all_divisors3(a));
        c = sum(all_divisors3(b));
        if a != b && a == c
            s[a] = 1;
            s[b] = 1;
        end
    end
    # return sum(keys(s).|>i->parse(Int,i))
    return keys(s)|>sum
end


#  Little more functional
# 0.12656805s
function euler21b()
    return sum(filter(v->v[1]!==v[2] && v[1] === v[3],(1:9999).|>a-> (
                  b = sum(all_divisors3(a));
                  c = sum(all_divisors3(b));
                  [a,b,c]
         )
         ).|>v->v[1])
end

# 0.07385759s
function euler21c()
    n = 9999
    s = zeros(Int,n)
    for i in 2:n
        s[i] = sum(all_divisors3(i))
    end
    a = []
    for i in 2:n
        # ignore perfect numbers...
        if s[i] <= n && i==s[s[i]] && i!= s[i]
            push!(a,i)
        end
    end
    return sum(a)
end


# run_euler(euler21a)
# run_euler(euler21b);
run_euler(euler21c);
