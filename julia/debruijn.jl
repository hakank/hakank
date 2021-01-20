#=

  de Bruijn sequences in Julia.

  See http://en.wikipedia.org/wiki/De_Bruijn_sequence
  """
  In combinatorial mathematics, a de Bruijn sequence of order n on 
  a size-k alphabet A is a cyclic sequence in which every possible 
  length-n string on A occurs exactly once as a substring (i.e., as 
  a contiguous subsequence). Such a sequence is denoted by B(k, n) 
  and has length kn, which is also the number of distinct strings of 
  length n on A. Each of these distinct strings, when taken as 
  a substring of B(k, n), must start at a different position, because 
  substrings starting at the same position are not distinct. Therefore, 
  B(k, n) must have at least kn symbols. And since B(k, n) has exactly kn 
  symbols, De Bruijn sequences are optimally short with respect to the 
  property of containing every string of length n exactly once. 
  """
  
  This is a port of the JavaScript version
  http://www.hakank.org/javascript_progs/debruijn.jl 
  which is a a port of (a part of) the original C program with
  the following copyright:

    -----------------------------------------------------------------------------
    | C program to generate necklaces, Lyndon words, and De Bruijn              |
    | sequences.  The algorithm is CAT and is described in the book             |
    | "Combinatorial Generation."  This program, was obtained from the          |
    | (Combinatorial) Object Server, COS, at http://www.theory.csc.uvic.ca/~cos |
    | The inputs are n, the length of the string, k, the arity of the           |
    | string, and density, the maximum number of non-0's in the string.         |
    | The De Bruijn option doesn't make sense unless density >= n.              |
    | The program can be modified, translated to other languages, etc.,         |
    | so long as proper acknowledgement is given (author and source).           |
    | Programmer: Frank Ruskey (1994), translated to C by Joe Sawada            |
    -----------------------------------------------------------------------------

  Note: Two adjustments has been done to the algorithm cited above 
  - adjusted to 1-based 
  - moved the global variables to a dict ("s")

  Compare with my web based programs:
  - http://www.hakank.org/comb/debruijn.cgi   
  - http://www.hakank.org/comb/deBruijnApplet.html
  - http://www.hakank.org/javascript_progs/debruijn.html


  Syntax: 
    $ julia debruijn.jl <k> <n> <p>
      where
       n: the language: 0..n-1
       k: length of each sub sequence
       p: if a flatten version should be printed as well

  Examples:

    $ julia debruijn.jl 2 3 1
    args: [ 2, 3, 1 ]
    k: 2  n: 3 , i.e. language: 0..k-1 (0..1) and with string len n (3)
    0 
    0 0 1 
    0 1 1 
    1 

    As a flat sequence: 0 0 0 1 0 1 1 1 (0 0)
    L.length: 8
    length:  2 ** 3+3-1 =  8 + 2 = 10


    The port code / door lock sequence: digits 0..9 of length 4
    $ julia debruijn.jl 10 4 1
    args: [ 10, 4, 1 ]
    k: 10  n: 4 , i.e. language: 0..k-1 (0..9) and with string len n (4)
    0 
    0 0 0 1 
    0 0 0 2 
    0 0 0 3 
    0 0 0 4 
    ...
    As a flat sequence: 0 0 0 0 1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0 5 0 0 0 6 0 0 0 7 0 0 0 8 0 0 0 9 0 0 1 1 0 0 1 2 0 
    ...
    8 9 8 7 8 9 9 7 9 7 9 8 8 7 9 8 9 7 9 9 8 7 9 9 9 8 8 8 8 9 8 8 9 9 8 9 8 9 9 9 9 (0 0 0)
    L.Julia: julia0
    length: 10**4+4-1 = 10000 + 3 = 10003

    (See http://www.hakank.org/comb/debruijn_k_10_n_4.html )


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("jl_utils.jl")

function pp(str)
    print(join(str,""))
end

#
# debruijn(k=2,n=3,print_seq=1)
#
# k: language (0..k-1)
# n: string length
# print_seq: also print the sequence as a flat list?
#
#
# the door lock combination solution
# is k = 10, n = 4
# Also see: http://www.hakank.org/comb/debruijn_k_10_n_4.html
# 
function debruijn(k=2,n=3,print_seq=1)
    
    println("k:$k n:$n i.e. language: 0..k-1 (0..$(k-1)) and with string len n ($n)")

    L = Int[]
    a = zeros(Int,n+2)

    # Variables to move around (instead of global variables)
    s = Dict(
        :k=>k,
        :n=>n,
        :L=>L,
        :a=>a,
    )

    Gen(1,1,0, s)
    L = s[:L] # get the changed version

    # print as a flat sequence
    if print_seq > 0
        pp("\nAs a flat sequence: ")
        pp(join(L," "))
        # and wrap the first n-1 items
        pp(" (")
        pp(join(L[1:n-1]," "))
        pp(")")
        println()
        println("L.length:",length(L))
    end
    println("length: k^n+n-1 = $(k^n) + $(n-1) = $(k^n+n-1)")

    return L
end


#
# When to print a digit
#
function Print(p,s)
    if s[:n] % p === 0
        for j in 1:p
            t = s[:a][j+1]
            push!(s[:L],t)
            pp(t)
            pp(" ")
        end
        println()
    end
end


function Gen(t, p, ones,s)
    n = s[:n]
    if ones <= n
        if t > n
            Print(p,s)
        else
            a = s[:a]; k = s[:k]    
            a[t+1] = a[t-p+1]
            if a[t+1] > 0
                Gen(t+1,p,ones+1,s)
            else
                Gen(t+1,p,ones,s)
            end
            for j = a[t-p+1]+1:k-1
                a[t+1] = j
                Gen(t+1,t,ones+1,s)
            end
            s[:a] = a 
        end
    end 
end

args = parse.(Int,ARGS)

# For the door lock problem:
#  k = 10, n = 4

# k: language (0..k-1)
k = length(args)>0 ? args[1] : 2 # 10

# n: string length
n = length(args)>1 ? args[2] : 3 # 4

# also print the sequence as a flat list?
print_seq = length(args) > 2 ? args[3] : 1

L = debruijn(k, n, print_seq);