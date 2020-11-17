# using Memoization
using Printf

function run_euler(p)
  t = @timed p()
  @printf "%s: %d: %2.8fs\n" p t.value t.time
end

# @memoize function fib(n)
#    n < 3 ? 1 : fib(n-1) + fib(n-2)
# end

function fib_nonmem(n)
    n < 3 ? 1 : fib(n-1) + fib(n-2)
end

# For Fibonacci with BigInt
# Note: @memoize is too slow, probably because of BigInts
# (or that I have not used @memoize correctly)
fibmemBigInt_dict = Dict{Int,BigInt}()
function fibmemBigInt(n)
    get!(fibmemBigInt_dict, n) do
        n < 3 ? 1 : fibmemBigInt(BigInt(n-1)) + fibmemBigInt(BigInt(n-2))
    end
end


function isPrime(n::Int)
    if n < 2
        return false
    end
    if n === 2 || n === 3
        return true
    end

    if n % 2 === 0
        return false
    end

    m = 1+ceil(Int,sqrt(n))
    for i in 3:2:m
        if n % i === 0
            return false;
        end
    end
    return true;
end

# @memoize function isPrimeCached(n)
#    return isPrime(n)
# end

# return all primes <= n
function primes(n::Int)
    primes = [2]
    for i in 3:2:n
        if isPrime(i)
            push!(primes,i)
        end
    end
    return primes;
end

#
# returns the prime divisors of n.
#

function prime_divisors(n::Int)
    if n === 1
        return []
    end
    divisors = []
    while (n > 1)
        m = round(Int,ceil(sqrt(n)))
        for i in 2:m
            if n % i == 0 && isPrime(i)
                push!(divisors,i)
                n /= i
                continue
            end
        end
    end
    return divisors
end

# Find the primes <= n
function sieve(n::Int)
    a = Array(range(1,length=n))
    a[1] = 0;
    a[2] = 2;
    for i in 2:ceil(Int,sqrt(n))
        for j in i*i:i:n
            a[j] = 0
        end
    end
    return filter(i->a[i]>0,1:n);

end


# Prime factors of n
function factors(n::Int)
    if n == 1
        return [1]
    end
    f = []
    while n % 2 == 0
        push!(f,2)
        n /= 2
    end
    t = 3
    while n > 1 && t < ceil(sqrt(n))
        while n % t == 0
            push!(f,t)
            n /= t
        end
        t += 2
    end
    if n > 1
        push!(f,round(Int,n))
    end
    return f
end


# Is n a palindromic number?
function palindromic_number(n::Int)
    s = string(n)
    len = length(s)
    for i in 1:ceil(Int,len/2)
        if s[i] != s[len-i+1]
            return false
        end
    end
    return true
end

# is the list a palindrom?
function palindromic_list(s)
    # return s == reverse(s)
    len = length(s)
    for i in 1:ceil(Int,len/2)
        if s[i] != s[len-i+1]
            return false
        end
    end
    return true
end


# All divisors of a number, except 1 and n
function all_divisors(n::Int)
    divisors = [];
    m = round(Int,n / 2)
    for i in 2:m
        if n % i == 0
            push!(divisors,i)
        end
    end
    return divisors;

end


# // All divisors of n, including 1 and n
function all_divisors2(n::Int)
    return hcat([1], all_divisors(n),[n])
end


# All divisors of n, including 1 (but not n)
function all_divisors3(n::Int)
    divisors = [1]
    m = round(Int,n / 2)
    for i in 2:m
        if n % i == 0
            push!(divisors,i)
        end
    end
    return divisors
end


# convert a list of elements in a to a hash with
#   { element: number of occurrences of element, ...}
function collect_list(a)
    m = Dict()
    a.|>(e->
        if !haskey(m,e)
            m[e] = 1
        else
            m[e] += 1
        end

    )
    return m
end

# next permutation of array p
function next_permutation(p)
    perm = p
    n = length(perm)
    k = n - 1
    while perm[k] > perm[k+1] && k >= 0
        k = k - 1
    end
    if k > 0
        j = n
        while perm[k] > perm[j]
            j = j - 1
        end
        perm[k],perm[j] = perm[j], perm[k]
        r = n
        s = k + 1
        while r > s
            perm[r],perm[s] = perm[s],perm[r]
            r = r - 1
            s = s + 1
        end
    end
    return perm
end

#
# Generate all permutations
# This is much faster than all_permutations2
#
function all_permutations(s)
    perms = []
    push!(perms,s)
    ss = copy(s)
    p = next_permutation(copy(s));;
    push!(perms,p)
    while true
        p = next_permutation(p)
        # Note: I have top copy p here
        push!(perms,copy(p))
        if p == reverse(s)
            break
        end
    end
    return perms;
end


# Inspired by an answer from
# http://stackoverflow.com/questions/4240080/generating-all-permutations-of-a-given-string
# This is too slow. Use all_permutations instead
function all_permutations2(s)
    perms = [];
    permutation_tmp([], s, perms);
    return perms;
end

function permutation_tmp(prefix,s, perms)
    n = length(s)
    if n == 0
        push!(perms,prefix)
    else
        for i in 1:n
            permutation_tmp(vcat(prefix,s[i]), vcat(s[1:i-1],s[i+1:n]), perms)
        end
    end
end


# Convert a number to its (integer) digits
# Note: this should be replaced with
#  digits(n)
function num_to_list(n::Int)
    # return split(string(n),"").|>i->parse(Int,i)
    return digits(n)
end

# length of a number (i.e. the number of digits)
function nlen(n::Int)
    return floor(Int,log10(n))+1
end

# check is s is a pandigital number  1..9
function is_pandigital(s)
    return length(s) === 9 && !occursin("0",s) && length(Set(s)) == 9
end
