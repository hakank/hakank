#=

  Euler #35 in Julia.

  Problem 35
  """
  The number, 197, is called a circular prime because all rotations
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100:
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

# Rotate an array/string
function rotate1(n,i)
    s = string(n)
    ii = i+1
    return parse(Int,join(vcat(s[ii:end],s[1:ii-1])))
end

function undigit(d; base=10)
    s = zero(eltype(d))
    mult = one(eltype(d))
    for val in d
        s += val * mult
        mult *= base
    end
    return s
end

# This is slightly faster
@inline function rotate(n,i)
    s = digits(n)
    ii = i+1
    d = vcat(s[ii:end],s[1:ii-1])
    return undigit(d)
end


#
# Note: It's a little faster when using prime_set as
#       a parameter for is_circular_prime than
#       relying of the global prime_set in the function.
#       That's surprising...
#
function is_circular_prime(n, prime_set)
    # s = split(string(n),"") # num_to_list(n)
    # len = length(s)
    len = nlen(n)
    v = 0
    for i = 1:len
        v = rotate(n,i)
        if !(v in prime_set)
            return false;
        end
    end
    return v in prime_set
end

# slightly slower than is_circular_prime
function is_circular_prime2(n, prime_set)
    s = split(string(n),"") # num_to_list(n)
    len = length(s)
    v = n
    for i = 2:len
        v = parse(Int,join(vcat([s[j] for j in i:len], [s[j] for j in 1:i-1]),""))
        if !(v in prime_set)
            return false;
        end
    end
    return v in prime_set
end

# Faster if global
# Ah, it's because the time of constructing it 
# is not counted in the run time!
prime_set = Set(primes(1_000_000))

# 0.08751774s
function euler35a()
    # prime_set = Set(primes(1_000_000))
    numCircularPrimes = 0
    for n in prime_set
        if is_circular_prime(n,prime_set)
            numCircularPrimes += 1
        end
    end

    return numCircularPrimes
end

# 0.11947861s
function euler35b()
    # prime_set = Set(primes(1_000_000))
    return [1 for n in prime_set if is_circular_prime(n,prime_set)]|>length
end

# Using is_circular_prime2
# 0.24161784s
function euler35c()
    # prime_set = Set(primes(1_000_000))
    # return filter(n->is_circular_prime2(n,prime_set), prime_set)|>length
    return [1 for n in prime_set if is_circular_prime2(n,prime_set)]|>length
end


run_euler(euler35a)
# run_euler(euler35b)
# run_euler(euler35c)
