#!/usr/bin/python -u
# -*- coding: latin-1 -*-
#
# Here are my solutions of the first 50 Project Euler problems.
# Note: I tend to have a 1s rule, i.e. that the problems should
# (each) be solved in < 1s. Also that all (together) should be
# run in < 10s. The last goal has been achieved but not the first:
# there are some that takes > 1s, but below 2s so it's acceptable.
#
#
# Please note that it's somewhat experimental...
#
# 
# Note:
#   pypy solves most problems much faster. Unfortunately, pypy don't
#   support ortools.* so it can't be used generally.
#   Instead, for these problems, I run an alternative (and in general slower)
#   version.
#   Run as
#     $ time pypy euler.py
#
#   Running all 1..50 using pypy takes 3.4s(!), compared
#   with ~10s (9.8 as of writing) using plain Python 2.7.3.
#   (Interesting: when using `pypy --jit off` then it's much slower: 47s!)
#
# Profiling
#   $ time python -m cProfile -s time euler.py euler14c
#
# Syntax:
#    -  $ time python euler.py
#       Runs all 1..50 problem and show the times, statistics etc
#    -  $ time python euler.py euler1
#       Run just one problem (here euler1)#
#    -  $ time python euler.py all
#       Runs _all variants_ of problems 1..50 with a timeout of 3s.
#       Shows times, statistics etc.
#
#
#
# Hakan Kjellerstrand, hakank@gmail.com
# http://www.hakank.org/
#
#
from __future__ import print_function
import sys, string, re, os, re
import time, signal
import collections, functools, itertools
import timeit
from math import *
from operator import mul
from fractions import gcd
    
#
# can we use cp (ortools)?
#
use_cp = [1]

try:
   from ortools.constraint_solver import pywrapcp as cp
except:
   print("No ortools.constraint_solver supported (no substitution given)")
   use_cp = [0]


bad = []
very_bad = []
errors = []
all_results = {}
total_time = [0] 
answers = {}
got_answers = [0]

#
# TODO: These takes > 1s (using plain Python 2.7.3)
#   #14: 1.80s (euler14c)  (using pypy: 0.52s)
#   #23: 1.04s             (using pypy: 0.24s)
#   #27: 1.06s             (using pypy: 0.33s)
#   #30: 1.36s (euler30b)  (using pypy: 0.23s)
#   #37: 0.78s (fixed!) #  1.86s 
#


# From https://wiki.python.org/moin/PythonDecoratorLibrary#Memoize
class memoized(object):
   '''Decorator. Caches a function's return value each time it is called.
   If called later with the same arguments, the cached value is returned
   (not reevaluated).
   '''
   def __init__(self, func):
      self.func = func
      self.cache = {}
   def __call__(self, *args):
      if not isinstance(args, collections.Hashable):
         # uncacheable. a list, for instance.
         # better to not cache than blow up.
         return self.func(*args)
      if args in self.cache:
         return self.cache[args]
      else:
         value = self.func(*args)
         self.cache[args] = value
         return value
   def __repr__(self):
      '''Return the function's docstring.'''
      return self.func.__doc__
   def __get__(self, obj, objtype):
      '''Support instance methods.'''
      return functools.partial(self.__call__, obj)

# alternative
def memoized2(obj):
   """Here's a modified version that also respects kwargs. """
   cache = obj.cache = {}
   @functools.wraps(obj)
   def memoizer(*args, **kwargs):
      key = str(args) + str(kwargs)
      if key not in cache:
         cache[key] = obj(*args, **kwargs)
      return cache[key]
   return memoizer


@memoized2
def fib(n):
    if n <= 1: return 1
    return fib(n-1)+fib(n-2)


# use this for plain system time
# without the timeit wrapper (which has some overhead...)
def bench1(f):
   s = str(f)
   m = re.search(r"<function (\w+) at.+>", s)
   if m:
      func = m.group(1)
   else:
      func = f
   print(func, ": ")
   # exec(func + "()")
   eval(func+"()")
   sys.stdout.flush()   


# bench a problem with time reported
def bench(f):
   func = f.__name__
   # print func, ": ",
   # sys.stdout.flush()
   ff = func + "()"
   # print ff
   # timer = timeit.Timer(ff, "from __main__ import " + func)
   # t = timer.timeit(1)
   # t = timeit.timeit(f,number=1)
   _ff,fnum,_rest=re.split("(\d+)", func)
   t1 = time.time()
   answer = f()
   t2 = time.time()
   t = t2 - t1
   
   total_time[0] += t
   message = ""
   if t >= 2.0:
      message = "!!! FIX"
      very_bad.append([func,t])
   elif t > 1.0:
      message = "!"
      bad.append([func,t])
   all_results[func] = t
   print("%-10s: %-20s%f%5s" % (func, str(answer), t, message))
   if got_answers[0]:
      a = answers[fnum]
      if str(answer) != a:
         print("\t!!!answer was %s. should be %s" % (str(answer), str(a)))
         errors.append(func)  
   sys.stdout.flush()


def euler1():
    """
    If we list all the ntural numbers below 10 that are multiples of 3 or 5, 
    we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
    """
    # return sum([i for i in range(999+1) if i % 3 == 0 or i % 5 == 0])
    return sum(i for i in range(999+1) if i % 3 == 0 or i % 5 == 0)    


def euler2():
    """
    Each new term in the Fibonacci sequence is generated by adding the 
    previous two terms. By starting with 1 and 2, the first 10 terms will be:

    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
    
    Find the sum of all the even-valued terms in the sequence which do not 
    exceed four million.
    """
    # print sum([fib(n) for n in range(1,100) if fib(n) < 4000000 and fib(n) & 1 == 0])
    return sum(fib(n) for n in range(1,100) if fib(n) < 4000000 and fib(n) & 1 == 0)    

# (no cheating with the hardcoded range)
def euler2b():
    i = 1
    s = 0
    f = fib(i)
    while f < 4000000:
        if f % 2 == 0:
            s += f
        i += 1
        f = fib(i)
    return (s)

#
# using .append()
#
def euler2c():
   f = [1,1]
   while f[-1] < 4000000:
      f.append(f[-1]+f[-2])
   f.remove(f[-1]) # remove last since it's too high
   return sum(filter(lambda i: i % 2 == 0, f))
    

# @memoized
def alldivisors(n,div):
   m = n
   divisors = []
   while m % div == 0:
      divisors.append(div)
      m //= div
   return divisors, m

# @memoized
def factors(n):
     m = n
     factors = []
     while m & 1 == 0:
         factors.append(2)
         # m = m // 2
         m >>= 1
     t = 3
     while m > 1 and t < 1+(sqrt(m)):
        if m % t == 0:
           divisors, newm = alldivisors(m, t)
           factors += divisors
           # factors.append(*divisors)
           m = newm
        t += 2
     if m > 1: factors.append(m)
     return factors

def euler3():
    """
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
    """
    return max(factors(600851475143))


def palindromic(n):
    s = str(n)
    return s == s[::-1]  # reverse

def euler4():
    """
    A palindromic number reads the same both ways. The largest palindrome made 
    from the product of two 2-digit numbers is 9009 = 91 × 99.

    Find the largest palindrome made from the product of two 3-digit numbers.
    """
    m = 0
    f = 100
    t = 999
    for i in range(f,t):
        for j in range(i,t):
            ij = i*j
            if ij > m and palindromic(ij):
                m = ij
    return m

# using comprehension, slower
def euler4b():
    return max([i*j for i in range(100,999) for j in range(i,999) if palindromic(i*j)])



# Least common multiple
def lcm(*numbers):
    """Return lowest common multiple."""
    def lcm(a, b):
        return (a * b) // gcd(a, b)
    return functools.reduce(lcm, numbers, 1)


def euler5():
    """
    2520 is the smallest number that can be divided by each of the numbers 
    from 1 to 10 without any remainder.
    
    What is the smallest number that is evenly divisible by all of the numbers 
    from 1 to 20?
    """
    return lcm(*[i for i in range(2,21)])

        
def euler6():
    """
    The sum of the squares of the first ten natural numbers is,
    1^(2) + 2^(2) + ... + 10^(2) = 385
    
    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
    
    Hence the difference between the sum of the squares of the first ten 
    natural numbers and the square of the sum is 3025 - 385 = 2640.
    
    Find the difference between the sum of the squares of the first one 
    hundred natural numbers and the square of the sum.
    """
    # return sum([i for i in range(1,101)])**2 - sum([i**2 for i in range(1,101)])
    return sum(i for i in range(1,101))**2 - sum(i**2 for i in range(1,101))    


# @memoized
def is_prime(n):
    if n < 2: return False
    if n == 2: return True
    # if n % 2 == 0:
    if not n & 1:       
        return False
    for i in range(3, 1+int(sqrt(n)), 2):
        if n % i == 0:
          return False
    return True

# an alternative version
def is_prime2(num):
    if num < 2:
        return False
    # if num % 2 == 0:
    if not num & 1:       
        return num == 2
    div = 3
    while div * div <= num:
        if num % div == 0:
            return False
        div += 2
    return True

# slower
def is_prime3(num):
   if num < 2:
      return False
   primes = {2,3,5,7,11,13}
   if num in primes:
      return True
   for p in primes:
      if num % p == 0:
         return False
   for i in range(17,1+int(sqrt(num)),2):
      if num % i == 0:
         return False
   return True

@memoized2
def is_prime_cached(n):
   return is_prime(n)


@memoized2
def is_prime2_cached(n):
   return is_prime2(n)


# Return the first n primes
def nprimes(n):
    primes = [2]
    i = 3
    while len(primes) < n:
        if is_prime(i):
            primes.append(i)
        i += 2
    return primes


# Return the primes below limit
def primes(limit):
   primes = [2]
   i = 3
   for i in range(3, limit, 2):
      if is_prime(i):
         primes.append(i)
   return primes

# slower than primes(limit)
def primes2(limit):
   primes = [2] + [i for i in range(3, limit, 2) if is_prime(i)] 
   return primes

 

# prime sieve below limit
def sieve(limit):
    a = [1] * limit
    a[0] = a[1] = 0
    for (i, isprime) in enumerate(a):
        if isprime:
            for n in range(i*i, limit, i):
                a[n] = 0
    return [i for i in range(limit) if a[i]]



# primes as a set
def primes_set(limit):
   return {i for i in range(2,limit+1) if is_prime(i)}

# faster than sieve
def primeseq(limit):
    numbers = range(3, limit+1, 2)
    half = (limit)//2
    initial = 4
    for step in range(3, limit+1, 2):
        for i in range(initial, half, step):
            numbers[i-1] = 0
        initial += 2*(step+1)

        if initial > half:
            return [2] + filter(None, numbers)

# faster sieve
# (inspired by something on the net)
def sieve2(n):
    sieve = [True] * n
    for i in range(3,int(n**0.5)+1,2):
        if sieve[i]:
            sieve[i*i::2*i]=[False]*((n-i*i-1)//(2*i)+1)
    return [2] + [i for i in range(3,n,2) if sieve[i]]



def euler7():
   """
   By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
   that the 6^(th) prime is 13.

   What is the 10001^(st) prime number?
   """
   # return nprimes(10001)[-1]

   # This is slightly faster than calling nprimes/1
   limit = 10001
   c = 1
   i = 3
   prime = i   
   while c < limit:
      if is_prime(i):
         prime = i
         c += 1
      if c == limit:
         break
      i += 2
       
   return prime


# product of a list
def product(list):
   if len(list) == 0: return 0
   return functools.reduce(lambda a,b: a*b, list)

# convert elements to int and product
def product2(list):
   if len(list) == 0: return 0
   return product([int(i) for i in list])


def euler8():
   """
   Find the greatest product of five consecutive digits in the 
   1000-digit number.
   ...
   """
   n = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
   # p = [product([int(j) for j in n[i:i+5]]) for i in range(len(n)-4)]
   p = [product2(n[i:i+5]) for i in range(len(n)-4)]    
   return max(p)



# slower than euler9b(): 1.13s
def euler9():
   """
   A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
   a^2 + b^2 = c^2
   
   For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
   
   There exists exactly one Pythagorean triplet for which a + b + c = 1000.
   Find the product abc.
   """
   P = 0
   for C in range(1,501):
      for B in range(1,C+1):
         for A in range(1,B+1): 
             if A + B + C == 1000 and A**2 + B**2 - C**2 == 0:
                P = A*B*C
                break

   return P

# using CP: 0.065s
def euler9b():
   if not use_cp[0]:
      print("No ortools.constraint_solver supported, running euler9 instead")
      return euler9()

   solver = cp.Solver("Euler9b")

   x = [solver.IntVar(1,500,"x") for i in range(3)]

   solver.Add(solver.Sum(x) == 1000)
   solver.Add(x[0]*x[0] + x[1]*x[1] == x[2]*x[2])
   # solver.Add(solver.Power(x[0],2) + solver.Power(x[1],2) == solver.Power(x[2],2))   

   # symmetry breaking
   solver.Add(x[0] <= 1000//3)
   solver.Add(x[0] <= x[1])
   solver.Add(x[1] <= x[2])

   db = solver.Phase(x, solver.CHOOSE_MIN_SIZE_LOWEST_MAX, solver.ASSIGN_MIN_VALUE)
   solver.NewSearch(db)
   num_solutions = 0
   sol = []
   if solver.NextSolution():
      num_solutions += 1
      sol = product([int(x[i].Value()) for i in range(3)]) 
   solver.EndSearch()
   return sol
     

# comprehension variant of euler9
def euler9c():
   return [A*B*C for C in range(1,501) for B in range(1,C+1) for A in range(1,B+1) if A + B + C == 1000 and A**2 + B**2 - C**2 == 0]



 

# 0.48s
def euler10():
   """ 
   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
   Find the sum of all the primes below two million.
   """
   # return sum(primes(2000000)) # 6.35s
   # return sum(sieve(2000000))  # much faster: 0.48s
   # return sum(primeseq(2000000)) # 0.34s
   return sum(sieve2(2000000)) # 0.08s   



#
# slices an array A [...] into slices of length SliceLen
# and returns a list List
#
def array_slice(a, slice_len):
   return [[a[J] for J in range(I,I+slice_len)] for I in range(1,len(a)-slice_len)]


# running prod of a list L1 with slice length RLen -> L2
def running_prod(L1, RLen):
   return [product(LL) for LL in array_slice(L1, RLen)]


def euler11():
   """
   In the 2020 grid below, four numbers along a diagonal line have 
   been marked in red.

   ...
   
   The product of these numbers is 26 x 63 x 78 x 14 = 1788696.
   
   What is the greatest product of four adjacent numbers in any direction 
   (up, down, left, right, or diagonally) in the 20 x 20 grid?
   """
   mat = [[ 8, 2,22,97,38,15, 0,40, 0,75, 4, 5, 7,78,52,12,50,77,91, 8],
          [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48, 4,56,62, 0],
          [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30, 3,49,13,36,65],
          [52,70,95,23, 4,60,11,42,69,24,68,56, 1,32,56,71,37, 2,36,91],
          [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
          [24,47,32,60,99, 3,45, 2,44,75,33,53,78,36,84,20,35,17,12,50],
          [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
          [67,26,20,68, 2,62,12,20,95,63,94,39,63, 8,40,91,66,49,94,21],
          [24,55,58, 5,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
          [21,36,23, 9,75, 0,76,44,20,45,35,14, 0,61,33,97,34,31,33,95],
          [78,17,53,28,22,75,31,67,15,94, 3,80, 4,62,16,14, 9,53,56,92],
          [16,39, 5,42,96,35,31,47,55,58,88,24, 0,17,54,24,36,29,85,57],
          [86,56, 0,48,35,71,89, 7, 5,44,44,37,44,60,21,58,51,54,17,58],
          [19,80,81,68, 5,94,47,69,28,73,92,13,86,52,17,77, 4,89,55,40],
          [ 4,52, 8,83,97,35,99,16, 7,97,57,32,16,26,26,79,33,27,98,66],
          [88,36,68,87,57,62,20,72, 3,46,33,67,46,55,12,32,63,93,53,69],
          [ 4,42,16,73,38,25,39,11,24,94,72,18, 8,46,29,32,40,62,76,36],
          [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74, 4,36,16],
          [20,73,35,29,78,31,90, 1,74,31,49,71,48,86,81,16,23,57, 5,54],
          [ 1,70,54,71,83,51,54,69,16,92,33,48,61,43,52, 1,89,19,67,48]]
   # rows
   max1 = max([max(running_prod(Row, 4)) for Row in mat])
   # columns
   max2 = max([max(running_prod(Column, 4)) for Column in zip(*mat)])
   # diag down
   max3 = max([max([product([mat[A+I][A+J] for A in range(4)]) for I in range(17)]) for J in range(17)])
   # diag up
   max4 = max([max([product([mat[I-A][J+A] for A in range(4)]) for I in range(4,20)]) for J in range(17)])
   return max([max1,max2,max3,max4])


   
def collect12(A):
   # return [sum([1 for J in A if J == I]) for I in set(A) ]
   return [sum(1 for J in A if J == I) for I in set(A) ]

def euler12():
   """
   The sequence of triangle numbers is generated by adding the natural numbers. 
   So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
   The first ten terms would be:
   
   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

   Let us list the factors of the first seven triangle numbers:

       1: 1
       3: 1,3
       6: 1,2,3,6
      10: 1,2,5,10
      15: 1,3,5,15
      21: 1,3,7,21
      28: 1,2,4,7,14,28

   We can see that the 7th triangle number, 28, is the first triangle number 
   to have over five divisors.

   Which is the first triangle number to have over five-hundred divisors?)
   """
   tlen = i = tnum = 0
   while tlen <= 500:
     i += 1
     tnum += i
     tlen = product([e+1 for e in collect12(factors(tnum))])
     # print tnum, tlen
  
   # print "tnum:",tnum, "len:", len
   return tnum




def euler13():
   """ 
   Work out the first ten digits of the sum of the following 
   one-hundred 50-digit numbers.
   37107287533902102798797998220837590246510135740250
   ....
   20849603980134001723930671666823555245252804609722
   53503534226472524250874054075591789781264330331690)
   """
   nums = [37107287533902102798797998220837590246510135740250,
   46376937677490009712648124896970078050417018260538,
   74324986199524741059474233309513058123726617309629,
   91942213363574161572522430563301811072406154908250,
   23067588207539346171171980310421047513778063246676,
   89261670696623633820136378418383684178734361726757,
   28112879812849979408065481931592621691275889832738,
   44274228917432520321923589422876796487670272189318,
   47451445736001306439091167216856844588711603153276,
   70386486105843025439939619828917593665686757934951,
   62176457141856560629502157223196586755079324193331,
   64906352462741904929101432445813822663347944758178,
   92575867718337217661963751590579239728245598838407,
   58203565325359399008402633568948830189458628227828,
   80181199384826282014278194139940567587151170094390,
   35398664372827112653829987240784473053190104293586,
   86515506006295864861532075273371959191420517255829,
   71693888707715466499115593487603532921714970056938,
   54370070576826684624621495650076471787294438377604,
   53282654108756828443191190634694037855217779295145,
   36123272525000296071075082563815656710885258350721,
   45876576172410976447339110607218265236877223636045,
   17423706905851860660448207621209813287860733969412,
   81142660418086830619328460811191061556940512689692,
   51934325451728388641918047049293215058642563049483,
   62467221648435076201727918039944693004732956340691,
   15732444386908125794514089057706229429197107928209,
   55037687525678773091862540744969844508330393682126,
   18336384825330154686196124348767681297534375946515,
   80386287592878490201521685554828717201219257766954,
   78182833757993103614740356856449095527097864797581,
   16726320100436897842553539920931837441497806860984,
   48403098129077791799088218795327364475675590848030,
   87086987551392711854517078544161852424320693150332,
   59959406895756536782107074926966537676326235447210,
   69793950679652694742597709739166693763042633987085,
   41052684708299085211399427365734116182760315001271,
   65378607361501080857009149939512557028198746004375,
   35829035317434717326932123578154982629742552737307,
   94953759765105305946966067683156574377167401875275,
   88902802571733229619176668713819931811048770190271,
   25267680276078003013678680992525463401061632866526,
   36270218540497705585629946580636237993140746255962,
   24074486908231174977792365466257246923322810917141,
   91430288197103288597806669760892938638285025333403,
   34413065578016127815921815005561868836468420090470,
   23053081172816430487623791969842487255036638784583,
   11487696932154902810424020138335124462181441773470,
   63783299490636259666498587618221225225512486764533,
   67720186971698544312419572409913959008952310058822,
   95548255300263520781532296796249481641953868218774,
   76085327132285723110424803456124867697064507995236,
   37774242535411291684276865538926205024910326572967,
   23701913275725675285653248258265463092207058596522,
   29798860272258331913126375147341994889534765745501,
   18495701454879288984856827726077713721403798879715,
   38298203783031473527721580348144513491373226651381,
   34829543829199918180278916522431027392251122869539,
   40957953066405232632538044100059654939159879593635,
   29746152185502371307642255121183693803580388584903,
   41698116222072977186158236678424689157993532961922,
   62467957194401269043877107275048102390895523597457,
   23189706772547915061505504953922979530901129967519,
   86188088225875314529584099251203829009407770775672,
   11306739708304724483816533873502340845647058077308,
   82959174767140363198008187129011875491310547126581,
   97623331044818386269515456334926366572897563400500,
   42846280183517070527831839425882145521227251250327,
   55121603546981200581762165212827652751691296897789,
   32238195734329339946437501907836945765883352399886,
   75506164965184775180738168837861091527357929701337,
   62177842752192623401942399639168044983993173312731,
   32924185707147349566916674687634660915035914677504,
   99518671430235219628894890102423325116913619626622,
   73267460800591547471830798392868535206946944540724,
   76841822524674417161514036427982273348055556214818,
   97142617910342598647204516893989422179826088076852,
   87783646182799346313767754307809363333018982642090,
   10848802521674670883215120185883543223812876952786,
   71329612474782464538636993009049310363619763878039,
   62184073572399794223406235393808339651327408011116,
   66627891981488087797941876876144230030984490851411,
   60661826293682836764744779239180335110989069790714,
   85786944089552990653640447425576083659976645795096,
   66024396409905389607120198219976047599490197230297,
   64913982680032973156037120041377903785566085089252,
   16730939319872750275468906903707539413042652315011,
   94809377245048795150954100921645863754710598436791,
   78639167021187492431995700641917969777599028300699,
   15368713711936614952811305876380278410754449733078,
   40789923115535562561142322423255033685442488917353,
   44889911501440648020369068063960672322193204149535,
   41503128880339536053299340368006977710650566631954,
   81234880673210146739058568557934581403627822703280,
   82616570773948327592232845941706525094512325230608,
   22918802058777319719839450180888072429661980811197,
   77158542502016545090413245809786882778948721859617,
   72107838435069186155435662884062257473692284509516,
   20849603980134001723930671666823555245252804609722,
   53503534226472524250874054075591789781264330331690]
   return str(sum(nums))[:10]


#
# Euler 14
#
def longest_seq(limit):
   lens = {}
   # s = set() # add set for faster lookup (test)
   max_len = 0
   max_n = 1
   for n in range(2,limit+1):
      m = n
      clen = 1
      while m > 1:
         # if lens.has_key(m): # using this 2.47s
         if m in lens: # using this: 2.2s
         # if m in s: # testing (not faster 2.33s)
            clen += lens[m] - 1
            m = 1
         else:
            m = hailstone(m) # using this takes 2.9
            # # using this takes 2.47s
            # # if m % 2 == 0: 
            # #    # m = m // 2 # 2.20s
            # #    m >>= 1 # 2.14s
            # #    # m //= 2 # 2.27s
            # # else:
            # #    m = 3*m+1
            # if m % 2 == 1: # swapping if/else 2.06s
            #    m = 3*m+1
            # else:
            #    # m = m // 2 # 2.14s
            #    m >>= 1 # 2.05s
            #    # m //= 2 # 2.19s

            clen += 1
      lens[n] = clen
      # s.add(n) # add to set lookup
      if clen > max_len:
         max_len = clen
         max_n = n
         # print max_len, max_n
   return max_n


# euler14 memoized: 11.6s, unmemoized: 2.9
# @memoized
def hailstone(n):
   if n % 2  == 0:
      return n // 2
   else:
      return 3*n+1

@memoized
def hailstone_memo(n):
   if n % 2  == 0:
      return n // 2
   else:
      return 3*n+1



def euler14():
   """
   The following iterative sequence is defined for the set of positive integers:

   n = n/2 (n is even)
   n = 3n + 1 (n is odd)
   
   Using the rule above and starting with 13, we generate the following 
   sequence:
   13 40 20 10 5 16 8 4 2 1
   
   It can be seen that this sequence (starting at 13 and finishing at 1) 
   contains 
   10 terms. Although it has not been proved yet (Collatz Problem), it is 
   thought that all starting numbers finish at 1.
   
   Which starting number, under one million, produces the longest chain?
   
   NOTE: Once the chain starts the terms are allowed to go above one million.)
   """
   return longest_seq(999999);


#
# This is a port of a C program (that takes 0.4s)
# This Python version takes much (much) longer (even when using pypy).
def euler14b():      
   longest = 0
   terms = 0
   i = 2
   j = 0
   # for i in range(1,1000000):
   while i < 1000000:
      j = i
      this_terms = 1
      while j > 1:
         this_terms += 1; 
         if this_terms > terms:
            terms = this_terms;
            longest = i 
      if j % 2 == 0:
        j /= 2
      else:
        j = 3 * j + 1
      i += 1

   return "longest: %d (%d)\n" % longest, terms


#
# without call to longest_seq (shaving some millis)
# This is the fastest version: ~1.8s (0.5s with pypy)
#
def euler14c():
   lens = {}
   max_len = 0
   max_n = 1
   limit = 1000000
   for n in range(2,limit):
      m = n
      clen = 1
      while m != 1:
         if m in lens:
            clen += lens[m] -1
            break
         else:
            # if m % 2 == 1:
            if m & 1:
               m = 3*m+1
               # m += (m<<1) + 1
            else:
               m >>= 1
               # m //= 2
            clen += 1
      lens[n] = clen
      if clen > max_len:
         max_len = clen
         max_n = n
         # print max_len, max_n
   return max_n


# using hailstone_memo
def euler14d():
   limit = 1000000
   max_len = 0
   max_n = 0
   for n in range(2,limit):
      m = n
      this_len = 1
      while m > 1:
         # print "\t", m
         m = hailstone_memo(m)
         this_len += 1
      if this_len > max_len:
         max_len = this_len
         max_n = n
         # print max_len, max_n
   return max_n

#
# Testing only odd numbers since they are more likey
# to contain long sequences (the next step is 3*m+1)
#
def euler14e():
   lens = {}
   max_len = 0
   max_n = 1
   limit = 1000000
   for n in range(3,limit,2):
      m = n
      clen = 1
      while m != 1:
         if m in lens:
            clen += lens[m] -1
            break
         else:
            if m & 1:
               m = 3*m+1
            else:
               m >>= 1
            clen += 1
      lens[n] = clen
      if clen > max_len:
         max_len = clen
         max_n = n
         # print max_len, max_n
   return max_n
  

def euler15():
   """
   Starting in the top left corner of a 2x2grid, there are 6 routes 
   (without backtracking) to the bottom right corner.
   
   How many routes are there through a 20x20 grid?
   """
   return product(range(21,40+1)) // product(range(2,20+1))



def euler16():
   """
   2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
   What is the sum of the digits of the number 2^1000?
   """
   # return sum([int(i) for i in str(2**1000)])
   return sum(map(int, str(2**1000)))      



def english(N):
   divs      = [1000000000, 1000000,  1000,       100]
   divnames  = ["billion", "million", "thousand", "hundred"]
   prefixes  = ["0", "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine",""]
   ordinals  = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth", 
                "fourteenth","fifteenth", "sixteenth", "seventeenth", 
                "eighteenth", "nineteenth"]
   cardinals = ["one", "two", "three", "four", "five", "six", "seven",
                "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

   sstr = ""
   printed = 0,
   if N < 0:
      sstr = "minus" + sstr,
      N = -N

   for i in range(1,len(divs)+1):
      D = N // divs[i-1]
      N = N % divs[i-1]
      if D:
         sstr += english(D) + divnames[i-1]
         printed = 1

   if N > 0 and printed == 1:
      sstr += "and"

   if N == 0:
      1 == 1 # dummy
   elif N > 19:
      D = N // 10
      N = N % 10
      sstr += prefixes[D-1] + "ty" + english(N)
   else:
      sstr += cardinals[N-1]
   return sstr


def euler17():
   """
   If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
   then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
   
   If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
   words, how many letters would be used?
  
   NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
   contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 
   "and" when writing out numbers is in compliance with British usage.
   """
   # return sum([len(english(i)) for i in range(1,1000+1)])
   return sum(len(english(i)) for i in range(1,1000+1))   




def pp19(Row, Column, Sum, Tri, M):
   if Sum > M["max_val"]:
      M["max_val"] = Sum
   Row += 1
   if Row < len(Tri):
      for I in range(2):
         pp19(Row,Column+I, Sum+Tri[Row][Column+I], Tri, M)


def euler18():
   """
   By starting at the top of the triangle below and moving to adjacent 
   numbers on the row below, the maximum total from top to bottom is 23.

   3
   7 4
   2 4 6
   8 5 9 3
   
   That is, 3 + 7 + 4 + 9 = 23.

   Find the maximum total from top to bottom of the triangle below:
   
   75
   95 64
   17 47 82
   18 35 87 10
   20 04 82 47 65
   19 01 23 75 03 34
   88 02 77 73 07 63 67
   99 65 04 28 06 16 70 92
   41 41 26 56 83 40 80 70 33
   41 48 72 33 47 32 37 16 94 29
   53 71 44 65 25 43 91 52 97 51 14
   70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
   63 66 04 68 89 53 67 30 73 16 69 87 40 31
   04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

   NOTE: As there are only 16384 routes, it is possible to solve this problem 
   by trying every route. However, Problem 67, is the same challenge with a 
   triangle containing one-hundred rows; it cannot be solved by brute force, 
   and requires a clever method! ;o)
   """
   Tri  = [[75],
           [95,64],
           [17,47,82],
           [18,35,87,10],
           [20, 4,82,47,65],
           [19, 1,23,75, 3,34],
           [88, 2,77,73, 7,63,67],
           [99,65, 4,28, 6,16,70,92],
           [41,41,26,56,83,40,80,70,33],
           [41,48,72,33,47,32,37,16,94,29],
           [53,71,44,65,25,43,91,52,97,51,14],
           [70,11,33,28,77,73,17,78,39,68,17,57],
           [91,71,52,38,17,14,91,43,58,50,27,29,48],
           [63,66, 4,68,89,53,67,30,73,16,69,87,40,31],
           [ 4,62,98,27,23, 9,70,98,73,93,38,53,60, 4,23]]
   M = {}
   M["max_val"] = 0
   pp19(0,0, Tri[0][0], Tri, M)
   return M["max_val"]



#
# Converting Julian date <-> Gregorian date from:
# http:%www.hermetic.ch/cal_stud/jdn.htm
#
def date2julian(Y,M,D):
   return ( 1461 * ( Y + 4800 + ( M - 14 ) // 12 ) )  //  4 +\
          ( 367 * ( M - 2 - 12 * ( ( M - 14 )  //  12 ) ) )  //  12 -\
          ( 3 * ( ( Y + 4900 + ( M - 14 )  //  12 )  //  100 ) )  //  4 +  D - 32075

#
# Julian date to Gregorian date
#
def julian2date(JD):
    L = JD + 68569
    N = ( 4 * L )  //  146097
    L = L - ( 146097 * N + 3 )  //  4
    I = ( 4000 * ( L + 1 ) )  //  1461001
    L = L - ( 1461 * I )  //  4 + 31
    J = ( 80 * L )  //  2447
    D = L - ( 2447 * J )  //  80
    L = J  // 11
    M = J + 2 - ( 12 * L )
    Y = 100 * ( N - 49 ) + I + L
    return [Y, M, D]


#
# Day of week, Sakamoto's method
# http:%en.wikipedia.org/wiki/Weekday_determination#Sakamoto.27s_Method
#
def dow(Y, M, D):
   T = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
   YY = Y
   if M < 3:
      YY -= 1
   return (YY + YY // 4 - YY // 100 + YY // 400 + T[M-1] + D) % 7


def euler19():
   """
   You are given the following information, but you may prefer 
   to do some research for yourself.

   * 1 Jan 1900 was a Monday.
   * Thirty days has September,
     April, June and November.
     All the rest have thirty-one,
     Saving February alone,
     Which has twenty-eight, rain or shine.
     And on leap years, twenty-nine.
   * A leap year occurs on any year evenly divisible by 4, but not 
     on a century unless it is divisible by 400.
   
   How many Sundays fell on the first of the month during the 
   twentieth century (1 Jan 1901 to 31 Dec 2000)?
   """
   Sum = 0
   for D in range(date2julian(1901,1,1),date2julian(2000,12,31)+1):
      DD = julian2date(D)
      if DD[2] == 1 and dow(DD[0],DD[1],DD[2]) == 0:
         Sum += 1
   return Sum





def euler20():
   """
   n! means n (n 1) ... 3 2 1

   Find the sum of the digits in the number 100!)
   """
   # return sum([int(i) for i in str(factorial(100))])
   return sum(map(int, str(factorial(100))))





# @memoized
def sum_divisors(N):
    D = int(sqrt(N))
    Sum = 1
    for I in range(2,D+1):
       if N % I == 0:
          Sum += I
          M = N // I
          if I != M:
             Sum += M
    return Sum


def euler21():
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
   S = {}
   for A in range(1,9999+1):
      B = sum_divisors(A)
      if A != B:
         C = sum_divisors(B)
         if A == C:
            S[A] = 1
            S[B] = 1
   return sum(S.keys())


# using set()
# (no discernible difference to euler21/1)
def euler21b():
   S = set()
   for A in range(1,9999+1):
      B = sum_divisors(A)
      if A != B:
         C = sum_divisors(B)
         if A == C:
            S.add(A)
            S.add(B)
   return sum(S)


def to_code(S):
   return [ord(C)-64 for C in S]


def euler22():
   """
   Using names.txt [http://projecteuler.net/project/names.txt] 
   (right click and 'Save Link/Target As...'), a 46K 
   text file containing over five-thousand first names, begin by sorting 
   it into alphabetical order. Then working out the alphabetical value 
   for each name, multiply this value by its alphabetical position in the 
   list to obtain a name score.

   For example, when the list is sorted into alphabetical order, COLIN, 
   which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
   the list. So, COLIN would obtain a score of 938 53 = 49714.
   
   What is the total of all the name scores in the file?)
   """
   names = sorted([name.replace('"','') for name in open('names.txt').read().split(',')]) 
   return sum((i+1)*sum(to_code(name)) for i, name in enumerate(names))

# ' (fix for the coloring Python mode)


# a variant of itertools.product where the we want ordered items
def product_ordered(*args, **kwds):
    # product('ABCD', 'xy') --> Ax Ay Bx By Cx Cy Dx Dy
    # product(range(2), repeat=3) --> 000 001 010 011 100 101 110 111
    pools = map(tuple, args) * kwds.get('repeat', 1)
    result = [[]]
    for pool in pools:
        result = [x+[y] for x in result for y in pool if x > y]
    for prod in result:
        yield tuple(prod)

def euler23():
   """
   A perfect number is a number for which the sum of its proper divisors 
   is exactly equal to the number. For example, the sum of the proper divisors 
   of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

   A number n is called deficient if the sum of its proper divisors is less than 
   n and it is called abundant if this sum exceeds n.

   As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
   that can be written as the sum of two abundant numbers is 24. By mathematical 
   analysis, it can be shown that all integers greater than 28123 can be written 
   as the sum of two abundant numbers. However, this upper limit cannot be reduced 
   any further by analysis even though it is known that the greatest number that 
   cannot be expressed as the sum of two abundant numbers is less than this limit.
   
   Find the sum of all the positive integers which cannot be written as the sum of 
   two abundant numbers.
   """
   Limit = 20161
   Arr = [1] * (Limit + 1)
   for I in range(2,Limit+1):
      for J in range(I*2,Limit,I):
         Arr[J] += I

   Abundant = [I for I in range(12,Limit+1) if Arr[I] > I] # 1.395s
   # Abundant = filter(lambda I: Arr[I] > I, range(12,Limit+1)) # 1.31s
   # Abundant = list(itertools.ifilter(lambda I: Arr[I] > I, range(12,Limit+1)))
   for A in Abundant:
      for B in Abundant:
         if A+B > Limit:
            break
         if A <= B: 
            Arr[A + B] = 0
         
   
   #return sum([I for I in range(Limit+1) if Arr[I]])
   return sum(I for I in range(Limit+1) if Arr[I])   
   # return sum(itertools.ifilter(lambda I: Arr[I], range(Limit+1)))

def euler23b():
  N = 20161
  Abundant = [A for A in range(1,N) if sum_divisors(A) > A]
  Vec = [0] * (N+1)
  for A in Abundant:
     for B in Abundant:
        if A >= B and A+B <= N:
           Vec[A+B] = 1
  # return sum([I for I in range(N+1) if not Vec[I]])
  return sum(I for I in range(N+1) if not Vec[I])  




def euler24():
   """
   A permutation is an ordered arrangement of objects. For example, 3124 is one 
   possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
   listed numerically or alphabetically, we call it lexicographic order. The 
   lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
   What is the millionth lexicographic permutation of the digits 
   0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
   """
   c = 1
   for p in itertools.permutations(range(10)):
      if c == 1000000:
         return "".join([str(i) for i in p])
         break
      c += 1

# inspired by a solution on the 'net (faster)
def euler24b():
   N = 999999
   P = 10
   Eli = [I % 10 for I in range(1,P+1)]
   Answer = []
   for I in range(1,P+1):
      F = factorial(P-I)
      D = N // F
      N %= F
      # Answer += [Eli[D-1]]
      Answer.append(Eli[D-1])
      Eli.remove(Eli[D-1]),
   return "".join([str(E) for E in Answer + Eli])




# @memoized
def fib_len(I):
   return len(str(fib(I)))


def euler25():
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
   
   What is the first term in the Fibonacci sequence to contain 1000 digits?)
   """
   Target = 1000
   FoundUpper = 0
   I = 1
   FibLen = 0
   Step = 43
   # Get the upper limit
   while FibLen < Target and FoundUpper == 0:
       FibLen = fib_len(Step*I)
       if  FibLen > Target:
         FoundUpper = I
       I += 1 # jump to the next step

   # Now check all numbers from Step*(FoundUpper-1) .. Step*FoundUpper
   # The target must be in that interval.
   Fib = Step*(FoundUpper-1)
   FibLen = fib_len(Fib)
   while FibLen < Target and Fib <= Step*FoundUpper:
      FibLen = fib_len(Fib)
      Fib = Fib + 1

   return Fib

# Brute force
def euler25b():
   F1 = 1
   F2 = 1
   Len = 1
   Ix = 2
   while Len < 1000:
      Tmp = F1
      F1 = F2
      F2 = Tmp + F1
      Len = len(str(F2))
      Ix += 1

   return Ix
   
def euler25c():
   I = 1
   Len = 0
   while Len < 1000:
      # Len = fib_len(I)
      Len = len(str(fib(I)))
      I += 1

   return I
   





#
# Get the length of the repeating cycle for 1/n
#
def get_rep_len(I):
    FoundRemainders = [0 for K in range(1,I+2)]
    Value = 1
    Position = 1
    while FoundRemainders[Value] == 0 and Value != 0:
        FoundRemainders[Value] = Position
        Value = (Value*10) % I
        Position = Position+1
    return Position-FoundRemainders[Value]

def euler26():
   """
   A unit fraction contains 1 in the numerator. The decimal representation of the 
   unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

   Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
   seen that 1/7 has a 6-digit recurring cycle.

   Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
   its decimal fraction part.
   """ 
   MaxLen = 0
   MaxD = 0
   for D in range(2,999+1):
      if is_prime(D):
         Len = get_rep_len(D)
         if Len > MaxLen:
            MaxLen = Len
            MaxD = D
   # return "maxD=", MaxD, "maxLen=",MaxLen
   return MaxD






def p27(A,B):
   N1 = 0
   PP = N1**2 + A*N1 + B
   while PP > 1 and is_prime(PP):
      N1 += 1
      PP = N1**2 + A*N1 + B
   return N1

def euler27():
   """
   Euler published the remarkable quadratic formula:

   n^2 + n + 41

   It turns out that the formula will produce 40 primes for the consecutive values 
   n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
   41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

   Using computers, the incredible formula  n^2 - 79n + 1601 was discovered, which 
   produces 80 primes for the consecutive values n = 0 to 79. The product of the 
   coefficients, -79 and 1601, is -126479.

   Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |-4| = 4

   Find the product of the coefficients, a and b, for the quadratic 
   expression that produces the maximum number of primes for consecutive 
   values of n, starting with n = 0.
   """
   T = 999
   BestLen = 0
   BestA = 0
   BestB = 0
   for A in range(-T,T+1,2):
      for B in range(-T,T+1,2):
            Len = p27(A,B)
            if Len > BestLen:
               BestLen = Len
               BestA = A
               BestB = B
   # return "besta:",BestA,"bestB:",BestB, " bestLen:", BestLen, "answer=", BestA*BestB
   return BestA*BestB   






def euler28():
   """
   Starting with the number 1 and moving to the right in a clockwise 
   direction a 5 by 5 spiral is formed as follows:
  
     21 22 23 24 25
     20  7  8  9 10
     19  6  1  2 11
     18  5  4  3 12
     17 16 15 14 13

   It can be verified that the sum of the numbers on the diagonals is 101.
  
   What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
   """
   S = 1
   N = 3
   while N <= 1001:
      S += 4 * N**2 - 6 * N + 6
      N += 2
   return S





def euler29():
  """
  Consider all integer combinations of a^b for 2 <= a <= 5 and 2 <= b <= 5:

      2^2=4, 2^3=8, 2^4=16, 2^5=32
      3^2=9, 3^3=27, 3^4=81, 3^5=243
      4^2=16, 4^3=64, 4^4=256, 4^5=1024
      5^2=25, 5^3=125, 5^4=625, 5^5=3125

  If they are then placed in numerical order, with any repeats removed, we get the 
  following sequence of 15 distinct terms:

  4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

  How many distinct terms are in the sequence generated by a^b for 
  2 <= a <= 100 and 2 <= b <= 100?
  """ 
  Min = 2
  Max = 100
  Hash = {}
  for A in range(Min,Max+1):
     for B in range(Min,Max+1):
        Hash[A**B] = 1
  return len(Hash.keys())


def euler29b():
   Min = 2
   Max = 100
   return len(set([A**B for A in range(Min,Max+1) for B in range(Min,Max+1)]))


  

def euler30():
  """
  Surprisingly there are only three numbers that can be written 
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of 
  fifth powers of their digits.
  """
  return (sum([N for N in range(10,6*9**5) if
              N == sum([int(I)**5 for I in str(N)])]))


  # return (sum(N for N in range(10,6*9**5) if
  #               N == sum(int(I)**5 for I in str(N))))


def euler30b():
  s = 0
  for N in range(10,6*9**5):
     # if sum([int(I)**5 for I in str(N)]) == N:
     if sum(int(I)**5 for I in str(N)) == N:                
        s += N
  return s




def coins(Coins, Money, M):
    Sum = 0
    Len = len(Coins)
    if M == Len:
      Sum = 1
    else:
       for I in range(M,Len+1):
         if Money - Coins[I-1] == 0:
            Sum += 1
         if Money - Coins[I-1] > 0:
            Sum += coins(Coins, Money-Coins[I-1], I)
    return Sum

def euler31():
  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """
  Coins = [200,100,50,20,10,5,2,1]
  return coins(Coins, 200, 1)


def scalar_product(solver, A,X,Product):
    solver.Add(Product == solver.Sum([A[I]*X[I] for I in range(len(A))]))

# CP approach (slower)
def euler31b():
   if not use_cp[0]:
      print("No ortools.constraint_solver supported, running euler31 instead")
      return euler31()

   solver = cp.Solver("Euler9b")
   coins = [200,100,50,20,10,5,2,1]
   Max = max(coins)
   n = len(coins)
   x = [solver.IntVar(0,Max) for i in range(n) ]
   # x = [solver.IntVar(0,Max // coins[i]) for i in range(n)] # smaller domains (slower)
   solver.Add(200 == solver.Sum([coins[i]*x[i] for i in range(n)])) # scalar product


   db = solver.Phase(x, solver.CHOOSE_FIRST_UNBOUND, solver.ASSIGN_CENTER_VALUE)
   solver.NewSearch(db)
   num_solutions = 0
   while solver.NextSolution():
      num_solutions += 1
   solver.EndSearch()

   return num_solutions
   



def euler32():
   """
   We shall say that an n-digit number is pandigital if it makes use of 
   all the digits 1 to n exactly once; for example, the 5-digit number, 
   15234, is 1 through 5 pandigital.

   The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
   containing multiplicand, multiplier, and product is 1 through 9 
   pandigital.

   Find the sum of all products whose multiplicand/multiplier/product 
   identity can be written as a 1 through 9 pandigital.
   HINT: Some products can be obtained in more than one way so be sure 
   to only include it once in your sum.
   """
   # c = 0
   #for p in itertools.permutations(range(10)):
   #   c += 1
   # Sum = 0
   ProdHash = {}
   for A in range(2,98+1):
      AS = str(A)
      for B in range(A+1,9876+1):
         Prod = A*B
         L = AS + str(B) + str(Prod)
         if len(L) == 9 and not "0" in L:
            Hash = set(L)
            if len(Hash) == 9 and not Prod in ProdHash:               
               ProdHash[Prod] = 1

   return sum(ProdHash.keys())

#
# CP:
# converts a number (s) <-> an array of integers (t)
# in the specific base.
#
def to_num(solver, t, s, base):
    tlen = len(t)
    solver.Add(s == solver.Sum([(base**(tlen-i-1))*t[i] for i in range(tlen)]))


def pandigital(cp, base=10, start=1, len1=1, len2=4):
    
    solver = cp.Solver("pandigital")

    # data
    max_d   = base-1
    x_len   = max_d + 1 - start
    max_num = base**4-1

    # decision variables
    num1 = solver.IntVar(0, max_num, 'num1')
    num2 = solver.IntVar(0, max_num, 'num2')
    res  = solver.IntVar(0, max_num, 'res')

    x = [solver.IntVar(start, max_d, 'x[%i]' % i) for i in range(x_len)]

    # constraints
    solver.Add(solver.AllDifferent(x))

    to_num(solver, [x[i] for i in range(len1)], num1, base)
    to_num(solver, [x[i] for i in range(len1,len1+len2)], num2, base)
    to_num(solver, [x[i] for i in range(len1+len2,x_len)], res, base)

    solver.Add(num1*num2 == res)

    # no number must start with 0
    solver.Add(x[0] > 0)
    solver.Add(x[len1] > 0)
    solver.Add(x[len1+len2] > 0)

    # symmetry breaking
    solver.Add(num1 < num2)

    db = solver.Phase(x,solver.INT_VAR_SIMPLE, solver.INT_VALUE_DEFAULT)

    solver.NewSearch(db)
    num_solutions = 0
    solutions = []
    while solver.NextSolution():
       # we only return res (the product)
       solutions.append(int(res.Value()))
       num_solutions += 1

    solver.EndSearch()    
    return solutions


# Using CP
# This is an adaption of my pandigital model
#    http://www.hakank.org/or-tools/pandigital_numbers.py
def euler32b():
   if not use_cp[0]:
      print("No ortools.constraint_solver supported, using euler32 instead")
      return euler32()

   base = 10
   start = 1
   x_len = base-1 + 1-start
   solutions = []
   for len1 in range(1+(x_len)):
      for len2 in range(1+(x_len)):
         if x_len > len1 + len2:
            solution = pandigital(cp, base, start, len1, len2)
            if len(solution) > 0:
               solutions += solution

   return sum(set(solutions))
   


def euler33():
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
  S = 1
  for Y in range(1,9+1):
     for Z in range(Y,9+1):
        X = 9.0*Y*Z/(10.0*Y-Z)
        if floor(X)==X and Y/Z < 1.0 and X < 10.0:
           S = 1.0*(S*Y)/Z
  return int(1/S)
 



# @memoized
def fact_int(N):
   return factorial(int(N))
   
def euler34():
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """
  Sum = 0
  for N in range(10,100000+1):
     # if N == sum([fact_int(I) for I in str(N)]): # slower
     # if N == sum([factorial(int(I)) for I in str(N)]):
     if N == sum(factorial(int(I)) for I in str(N)):
        Sum += N
  return Sum

def euler34b():
  # return sum([N for N in range(10,100000+1) if N == sum([factorial(int(I)) for I in str(N)])])
  return sum(N for N in range(10,100000+1) if N == sum(factorial(int(I)) for I in str(N)))  
   
def rotate(l,n):
    return l[-n:] + l[:-n]

def rotate_int(l,n):
   return int(l[-n:] + l[:-n])

def is_circular_prime(P,Primes):
   S = str(P)
   Size = len(S)
   V = P
   for I in range(Size):
      if V in Primes:
         V = rotate_int(S,I)
   return V in Primes

def euler35():
   """
   The number, 197, is called a circular prime because all rotations 
   of the digits: 197, 971, and 719, are themselves prime.
   
   There are thirteen such primes below 100: 
   2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

   How many circular primes are there below one million?
   """
   # Primes = {P for P in sieve(1000000)}
   # Primes = {P for P in primeseq(1000000)} # 0.38s
   Primes = {P for P in sieve2(1000000)} # 0.27s
   NumCircularPrimes = 0
   for N in Primes:
      if is_circular_prime(N,Primes):
         NumCircularPrimes += 1

   return NumCircularPrimes


def euler35b():
   Primes = {P for P in primeseq(1000000)}   
   NumCircularPrimes = 0
   for P in Primes:
      S = str(P)
      Size = len(S)
      V = P
      for I in range(Size):
         if V in Primes:
            V = int(rotate(S,I))
      if V in Primes:
         NumCircularPrimes += 1

   return NumCircularPrimes


def reverse(s):
   return s[::-1]

def dec_to_base(N, Base):
   Res = []
   while N > 0:
       R = N % Base
       N //= Base
       Res += [R]
   return reverse(Res)


def euler36():
   """
   The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
   in both bases.
  
   Find the sum of all numbers, less than one million, which are palindromic 
   in base 10 and base 2.

   (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
   """
   Res = 0
   for N in range(1,999999+1):
      if palindromic(N):
         Bin = dec_to_base(N, 2)
         if Bin == reverse(Bin):
            Res += N

   return Res

# as a comprehension
def euler36b():
   # return sum([N for N in range(1,999999+1) if palindromic(N) and dec_to_base(N, 2) == reverse(dec_to_base(N, 2)) ])
   return sum(N for N in range(1,999999+1) if palindromic(N) and dec_to_base(N, 2) == reverse(dec_to_base(N, 2)) )   
   


def nlen(N):
   return int(log10(N))+1

def check37(N):
   OK = 1
   for I in range(1,len(str(N))):
     # t1 = int(s[I:]) # this approach is slower (~3s)
     # t2 = int(s[:I+1])
     II = 10**(I)
     t1 = N % II
     t2 = N // II
     if not(is_prime2(t1)) or not(is_prime(t2)):
        OK = 0
        break
   return OK == 1
  
def euler37():
   """
   The number 3797 has an interesting property. Being prime itself, it is possible to 
   continuously remove digits from left to right, and remain prime at each stage: 
   3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
   
   Find the sum of the only eleven primes that are both truncatable from left to right 
   and right to left.
   
   NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
   """
   # 2, 3, 5, and 7 are not considered truncable primes
   #  so we start on 9 
   P = 9
   Sum = 0
   C = 0
   while C < 11:
     if check37(P) and is_prime(P):
        C += 1
        Sum += P
     P += 2
   return Sum
  



def is_pandigital(S):
   return len(S) == len(set(S))

# 932718654
def euler38():
   """
   Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

   By concatenating each product we get the 1 to 9 pandigital, 
   192384576. We will call 192384576 the concatenated product of 192 
   and (1,2,3)

   The same can be achieved by starting with 9 and multiplying by 
   1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
   concatenated product of 9 and (1,2,3,4,5).

   What is the largest 1 to 9 pandigital 9-digit number that can be 
   formed as the concatenated product of an integer with 
   (1,2, ... , n) where n > 1?
   """
   MaxN = 0
   for N in range(9876,9,-1):
        S = str(N)
        I = 2
        while len(S) < 9:           
           S += str(N*I)
           I += 1
        if len(S) == 9 and S.find("0") == -1 and is_pandigital(S):
           MaxN = S
           break

   return MaxN
  
  



def euler39():
   """
   If p is the perimeter of a right angle triangle with integral length sides, 
   {a,b,c}, there are exactly three solutions for p = 120.
   
   {20,48,52}, {24,45,51}, {30,40,50}
   
   For which value of p <= 1000, is the number of solutions maximised?
   """
   N = 1000
   Squares = set(X*X for X in range(1,N+1))
   Valid = [(X,Y) for X in Squares for Y in Squares if 
            X < Y and (X+Y) in Squares and  (sqrt(X) + sqrt(X) + sqrt(X+Y)) < 1000]   
   Counts = collections.defaultdict(int)
   for (X2,Y2) in Valid:
      C = int(sqrt(X2) + sqrt(Y2) + sqrt(X2+Y2))
      Counts[C] += 1
   # find max count
   P = max([V for V in Counts.values()])
   # P = max(Counts.values())
   return [I for I in Counts if Counts[I] == P ][0]




def euler40():
   """
   An irrational decimal fraction is created by concatenating the positive integers:
   
   0.123456789101112131415161718192021...
   
   It can be seen that the 12th digit of the fractional part is 1.

   If dn represents the nth digit of the fractional part, find the 
   value of the following expression.
  
   d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
   """
   I = 1
   DLen = 1
   Prod = 1
   Index = 10 # Index = 10, 100, 1000, ..., 1000000
   while DLen <= 1000000:
       I += 1
       IStr = str(I)
       IStrLen = len(IStr)
       if DLen+IStrLen>=Index:
          Prod *= int(IStr[Index-DLen-1])
          Index *= 10
       DLen += IStrLen

   return Prod
   




def euler41():
   """
   We shall say that an n-digit number is pandigital if it makes use of all 
   the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
   and is also prime.

   What is the largest n-digit pandigital prime that exists?
   """
   # Simplification:
   # N=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
   # N=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
   # N = 9 # using N=9 as start value takes about 1.16s
   N = 7
   M = 0
   while M == 0 and N >= 4:
      P = reverse(range(1,N+1))
      # note: it's reversed sorted so we stop at first prime
      V = 4 # dummy value for the foreach loop
      for PP in itertools.permutations(P):
         V = int("".join([str(J) for J in PP]))
         if is_prime(V):
            M = V # found it
            break

      N -= 1

   return M





def triangle_number(N):
    return (N*(N+1)) // 2

def get_score42(Name):
   Total = 0
   for C in Name:
      Total += (ord(C)-64)
   return Total

def euler42():
   """
   The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
      so the first ten triangle numbers are:

   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

   By converting each letter in a word to a number corresponding to its 
   alphabetical position and adding these values we form a word value. For example, 
   the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
   is a triangle number then we shall call the word a triangle word.

   Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
   containing nearly two-thousand common English words, how many 
   are triangle words?
   """
   words = sorted([name.replace('"','') for name in open("words.txt").read().split(",")])
   t20 = {triangle_number(i) for i in range(1,20+1)}
   Len = len([w for w in words if get_score42(w) in t20])
   return Len
   



# CP approach
def euler43():
   """  
   The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
   each of the digits 0 to 9 in some order, but it also has a rather interesting 
   sub-string divisibility property.
  
   Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
   note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
   Find the sum of all 0 to 9 pandigital numbers with this property.
   """
   if not use_cp[0]:
      print("No ortools.constraint_solver supported, running euler43b instead")
      return euler43b()


   primes = [2,3,5,7,11,13,17]
   solver = cp.Solver("euler43")
   
   x = [solver.IntVar(0,9) for i in range(10)]

   solver.Add(solver.AllDifferent(x, True))
   for (i, p) in enumerate(primes):  
      solver.Add((100*x[i+1]+10*x[i+2]+x[i+3]) % p == 0)

   db = solver.Phase(x, solver.CHOOSE_MIN_SIZE_LOWEST_MAX, solver.ASSIGN_MIN_VALUE)
   solver.NewSearch(db)
   solutions = []
   while solver.NextSolution():
      r = int("".join([str(int(i.Value())) for i in x]))
      solutions.append(r)
   solver.EndSearch()
   return sum(solutions)

# slower
def euler43b():
   primes = [2,3,5,7,11,13,17]
   s = 0
   for p in itertools.permutations(range(10)):
      check = 1
      for (i, pp) in enumerate(primes):  
         if ((100*p[i+1]+10*p[i+2]+p[i+3]) % pp != 0):
            check = 0
            break
      if check:
         s += int("".join([str(int(i)) for i in p]))
   return s
      



def pent(N):
   return N*(3*N-1) // 2

def euler44():
   """  
   Pentagonal numbers are generated by the formula, P(n)=n(3n-1)/2. 
   The first ten pentagonal numbers are:

   1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

   It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, 
   their difference,  70 - 22 = 48, is not pentagonal.

   Find the pair of pentagonal numbers, P(j) and P(k), for which their sum 
   and difference is pentagonal and D = |P(k) - P(j)| is minimised; what 
   is the value of D?  
   """
   S = {pent(N) for N in range(1,2500+1)}
   D = 10000000
   for J in S:
      for K in S:
         if J < K:
            A = J+K
            if A in S and A < D:
               B = abs(J-K)
               if B in S and B < D:
                  D = B
   return D

# using map instead of set
def euler44b():
   S = {pent(N):1 for N in range(1,2500+1)}
   T = S.keys()
   D = 10000000
   for J in T:
      for K in T:
         if J < K:
            A = J+K
            if A in S and A < D:               
               B = abs(J-K)
               if B in S and B < D:                  
                  D = B
   return D


def euler44c():
   # S = {pent(N) for N in range(1,2500+1)}
   S = {pent(N):1 for N in range(1,2500+1)}   
   D = 10000000
   for (J,K) in itertools.product(S,repeat=2):
      if J < K:
         A = J+K
         if A in S and A < D:
            B = abs(J-K)
            if B in S and B < D:
               D = B
   return D


def euler45():
   """  
   Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

   Triangle 	  	Tn=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
   Pentagonal 	  	Pn=n(3n-1)/2 	  	1, 5, 12, 22, 35, ...
   Hexagonal 	  	Hn=n(2n-1) 	  	1, 6, 15, 28, 45, ...

   It can be verified that T(285) = P(165) = H(143) = 40755.

   Find the next triangle number that is also pentagonal and hexagonal.
   """
   def pent(N): return N*(3*N-1) // 2
   def tri(N):  return N*(N+1)  // 2
   def hexx(N): return N*(2*N-1)
   
   T = 285+1
   TT = tri(T)
   P = 165
   PP = pent(P)
   H = 143
   HH = hexx(H)
   while TT != PP or PP != HH:
      T += 1
      TT = tri(T)
      if TT > PP:
         P += 1
         PP = pent(P)
      if PP > HH:
         H += 1
         HH = hexx(H)
      if TT > HH:
         H += 1
         HH = hexx(H)
   return TT






def euler46():
   """  
   It was proposed by Christian Goldbach that every odd composite number can be 
   written as the sum of a prime and twice a square.

    9 =  7 + 2×1^2
   15 =  7 + 2×2^2
   21 =  3 + 2×3^2
   25 =  7 + 2×3^2
   27 = 19 + 2×2^2
   33 = 31 + 2×1^2

   It turns out that the conjecture was false.

   What is the smallest odd composite that cannot be written as the 
   sum of a prime and twice a square?
   """
   Res = 0
   for I in range(3,10000+1,2):
      if not is_prime(I):
         S = int(sqrt(I/2))
         Found = 0
         for J in range(1,S+1):
            Ts = J*J*2
            if is_prime(abs(I-Ts)):
               Found = 1
               break
         if Found == 0:
            Res = I
            break
   return Res


def euler47():
   """  
   The first two consecutive numbers to have two distinct prime factors are:

   14 = 2 x 7
   15 = 3 x 5

   The first three consecutive numbers to have three distinct 
   prime factors are:

   644 = 2^2 x 7 x 23
   645 = 3 x 5 x 43
   646 = 2 x 17 x 19.

   Find the first four consecutive integers to have four distinct primes 
   factors. What is the first of these numbers?
   """
   MaxN = 1000000
   F = [0] * MaxN
   for I in range(2,MaxN): 
      if F[I] == 0:
         for J in range(2*I,MaxN,I):
            F[J] += 1
   Goal = [4,4,4,4]
   for I in range(MaxN-3):
      if [F[J] for J in range(I,I+4)] == Goal:
         return I
         break





def euler48():
   """
   The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
   Find the last ten digits of the series, 
   1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
   """
   Sum = 0
   T = 10000000000
   for I in range(1,1000):
      N = I
      for J in range(2,I+1):
         N = (N * I) % T
      Sum = (Sum + N) % T
   return Sum

def euler48b():
   S = 1
   for I in range(2,1000+1):
      S += I**I
   SS = str(S)
   Len = len(SS)
   return "".join([SS[J] for J in range(Len-10,Len)])

def euler48c():
   S = str(sum(I**I for I in range(1,1000+1)))
   Len = len(S)
   return "".join([S[J] for J in range(Len-10,Len)])




def check_perms49(N, Diff):
  LL = []
  # AllPerms = list(itertools.permutations([int(I) for I in str(N)]))
  AllPerms = list(itertools.permutations(map(int,str(N))))
  if len(AllPerms) > 0:
     P1 = 0
     P2 = 0
     P1 = get_element49(N, AllPerms, Diff)
     if P1 > 0:
       P2 = get_element49(P1, AllPerms, Diff)
     if P2 > 0:
       LL = [N, P1, P2]
  return LL

def get_element49(N, LL, Diff):
  Res = 0
  for P in LL:
    PP = int("".join([str(I) for I in P]))
    if PP > N and PP-N == Diff:
      Res = PP
  return Res

def euler49():
   """  
   The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
   increases by 3330, is unusual in two ways: (i) each of the three terms are 
   prime, and, (ii) each of the 4-digit numbers are permutations of one another.

   There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
   exhibiting this property, but there is one other 4-digit increasing sequence.

   What 12-digit number do you form by concatenating the three terms 
   in this sequence?
   """
   Diff = 3330
   Primes = sieve2(10000)
   Res = 0
   for N in range(1001,9999+1,2):
      C = check_perms49(N, Diff)
      # if C != [] and N != 1487 and is_prime(N):
      if C != [] and N != 1487 and N in Primes:         
         Res = C

   return "".join([str(R) for R in Res])





def euler50():
   """
   The prime 41, can be written as the sum of six consecutive primes:
   41 = 2 + 3 + 5 + 7 + 11 + 13

   This is the longest sum of consecutive primes that adds to a prime 
   below one-hundred.

   The longest sum of consecutive primes below one-thousand that adds to a prime, 
   contains 21 terms, and is equal to 953.
  
   Which prime, below one-million, can be written as the sum of the most 
   consecutive primes?
   """
   N = 10000
   Primes = primes(N)
   Found = 0
   for Len in range(550,21,-1):
      for Offset in range(1,549+1):
         PP = sum([Primes[J] for J in range(Offset+1,Offset+Len+1)])
         if PP < 1000000 and is_prime2(PP):
            Found = PP
            break
      if Found:
         break
   return Found


#
# Sudoku
#
def sudoku(problem):
   """
   Solving a Sudoku problem
   """
   if not use_cp[0]:
      print("No ortools.constraint_solver supported (no substitution given)")
      return

   solver = cp.Solver("sudoku")
   
   cell_size = 3
   line_size = cell_size ** 2
   line = range(0, line_size)
   cell = range(0, cell_size)

   #
   # decision variables
   #
   grid = {}
   for i in line:
      for j in line:
         grid[(i, j)] = solver.IntVar(1, line_size)

   # initial values
   [solver.Add(grid[(i, j)] == problem[i][j])
            for i in line for j in line if problem[i][j]]
    
   # rows and columns
   for i in line:
      solver.Add(solver.AllDifferent([grid[(i, j)] for j in line]))
      solver.Add(solver.AllDifferent([grid[(j, i)] for j in line]))      


   # cells
   for i in cell:
      for j in cell:
         solver.Add(solver.AllDifferent([grid[(i * cell_size + di, j * cell_size + dj)]
                                             for di in cell for dj in cell]))

   # Regroup all variables into a list.
   grid_flat = [grid[(i, j)] for i in line for j in line]

   # Create search phases.
   db = solver.Phase(grid_flat,
                     solver.CHOOSE_FIRST_UNBOUND,
                     solver.ASSIGN_CENTER_VALUE)

   # solve
   solver.NewSearch(db)
   sol = []
   # while solver.NextSolution():
   # Since we know that the Sudokus are well formed, we just
   # take the first solution
   if solver.NextSolution():      
      sol = [[int(grid[(i, j)].Value()) for j in line] for i in line]
      # break
   solver.EndSearch()

   return sol


#
# This takes about 0.067s.
# Solving the test Sudoku takes about 0.02s.
#
def euler96():
   """
   Su Doku (Japanese meaning number place) is the name given to a popular puzzle
   concept. Its origin is unclear, but credit must be attributed to Leonhard Euler
   who invented a similar, and much more difficult, puzzle idea called Latin Squares.
   The objective of Su Doku puzzles, however, is to replace the blanks (or zeros)
   in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains
   each of the digits 1 to 9. Below is an example of a typical starting puzzle grid
   and its solution grid.

   ...

   A well constructed Su Doku puzzle has a unique solution and can be solved by logic,
   although it may be necessary to employ "guess and test" methods in order to eliminate
   options (there is much contested opinion over this). The complexity of the search
   determines the difficulty of the puzzle; the example above is considered easy because
   it can be solved by straight forward direct deduction.

   The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains
   fifty different Su Doku puzzles ranging in difficulty, but all with unique solutions
   (the first puzzle in the file is the example above).

   By solving all fifty puzzles find the sum of the 3-digit numbers found in the top
   left corner of each solution grid; for example, 483 is the 3-digit number found in
   the top left corner of the solution grid above.
   """
   if not use_cp[0]:
      print("No ortools.constraint_solver supported (and there is no ersatz)")
      return
      

   ## A test problem:
   # problem = [[0, 6, 0, 0, 5, 0, 0, 2, 0],
   #            [0, 0, 0, 3, 0, 0, 0, 9, 0],
   #            [7, 0, 0, 6, 0, 0, 0, 1, 0],
   #            [0, 0, 6, 0, 3, 0, 4, 0, 0],
   #            [0, 0, 4, 0, 7, 0, 1, 0, 0],
   #            [0, 0, 5, 0, 9, 0, 8, 0, 0],
   #            [0, 4, 0, 0, 0, 1, 0, 0, 6],
   #            [0, 3, 0, 0, 0, 8, 0, 0, 0],
   #            [0, 2, 0, 0, 4, 0, 0, 5, 0]]
   # return sudoku(problem)

   sudokus = []
   this_sudoku = []
   for line in open("sudoku.txt").readlines():
      m = re.search(r"Grid (\d+)", line)
      if m:
         if m.group(1) > 1 and this_sudoku != []:
            sudokus.append(this_sudoku)
            this_sudoku = []
      else:
         this_sudoku.append([int(d) for d in string.strip(line)])

   # the final
   sudokus.append(this_sudoku)

   values = 0
   for s in sudokus:
      sol = sudoku(s)
      values += int("".join([str(sol[0][i]) for i in range(3)]))
   
   return values


#
# Test all the best variants of problem 1..50.
#
def run_all():

   # Note: I don't want to show the answers in this program. So checking
   #       is done using a text file (euler_answers.txt) with the following
   #       structure:
   #           problemnum:answer
   #           problemnum:answer
   #           ....
   try:
      for a in [p.strip() for p in open("euler_answers.txt").readlines()]:
         fnum, correct = re.split(":",a)
         answers[fnum] = correct
      got_answers[0] = True
      print("Great, we got the answers to compare with.")
   except:
      print("Sorry, no answers to compare with.")
      pass
   
   bench(euler1) # 0.0002s
   
   # bench(euler2) # 0.0007s
   bench(euler2b) # 0.0003s
   
   bench(euler3) # 0.0003s
   
   bench(euler4) # 0.05s
   # bench(euler4b) # 0.2s
   
   bench(euler5) # 0.009s
   
   bench(euler6) # 0.00003s
   
   bench(euler7) # 0.12s
   
   bench(euler8) # 0.004s
   
   #bench(euler9)  # 1.14s
   bench(euler9b) # 0.12s
   #bench(euler9c) # 1.14s
   
   bench(euler10) # 0.08s
   
   bench(euler11) # 0.002s
   
   bench(euler12) # 0.59s
   
   bench(euler13) # 0.00003s
   
   # bench(euler14) # 2.47s!
   ##bench(euler14b) # MUCH TOO SLOW
   # bench(euler14c) # 1.80s! TODO FIX
   # bench(euler14d)
   bench(euler14e) # testing only odd numbers (1.6s)
   
   bench(euler15) # 0.00003s
   
   bench(euler16) # 0.0003s
   
   bench(euler17) # 0.009s
   
   bench(euler18) # 0.018s
   
   bench(euler19) # 0.042s
   
   bench(euler20) # 0.0001s
   
   bench(euler21) # 0.09s
   
   bench(euler22) # 0.01s
   
   bench(euler23) # 1.01 TODO FIX
   # bench(euler23b) # slower 1.7s
   
   #bench(euler24) # 0.13s
   bench(euler24b) # 0.00005s
   
   bench(euler25) # 0.02s
   # bench(euler25b) # 0.067s
   # bench(euler25c) # 0.076s
   
   bench(euler26) # 0.015s
   
   bench(euler27) # 1.07s! TODO FIX
   
   bench(euler28) # 0.002s
   
   #bench(euler29) # 0.0159s
   bench(euler29b) # slightly faster 0.0154s
   
   bench(euler30)  # 1.30s TODO FIX
   # bench(euler30b)  # 1.38s 
   
   bench(euler31) # 0.09s
   ## bench(euler31b) # slower 0.18s
   
   ## bench(euler32) # 0.51s
   bench(euler32b) # 0.027s
   
   bench(euler33) # 0.00006s
   
   #bench(euler34) # 0.32s
   bench(euler34b) # slightly faster 0.31s
   
   bench(euler35) # 0.29s
   # bench(euler35b) # 0.39s   
   
   bench(euler36) # 0.43s
   # bench(euler36b) #  # 0.48
   
   bench(euler37) # 0.002s (fixed, was 1.86s before)
   
   bench(euler38) # 0.001s
   
   bench(euler39) # 0.06s
   
   bench(euler40) # 0.067s
   
   bench(euler41) # 0.0003s
   
   bench(euler42) # 0.003s
   
   bench(euler43) # 0.12s
   ## bench(euler43b) # 3.0s
   
   ## bench(euler44) # 0.473s
   bench(euler44b) # 0.36s
   # bench(euler44c) # 0.46s
   
   bench(euler45) # 0.033s
   
   bench(euler46) # 0.019s
   
   bench(euler47) # 0.34s
   
   bench(euler48) # 0.042s
   # bench(euler48b) # 0.0120s
   ##bench(euler48c) # slightly slower 0.0123s
   
   bench(euler49) # 0.18s
   
   bench(euler50) # 0.11s


def timeout_handler(s,f):
   print("Timeout")
   raise Exception("Timeout")

#
# Test all the defined euler functions (with a timeout of 3s).
#
def run_all_all():
   esort = collections.defaultdict(list)
   for f in filter(lambda g: g.startswith("euler"), globals()):
      esort[int(re.split("(\d+)",f)[1])] += [f]
   times = {}
   timeout_time = 3 # seconds
   timeouts = []
   best = []
   worst = []
   for f in sorted(esort):
      if f > 50:
         break
      print("\nproblem #%i" % f)
      these_times = {}
      for func in esort[f]:
         print(func, )
         signal.signal(signal.SIGALRM, timeout_handler)
         signal.alarm(timeout_time)
         ff = func + "()"
         # timer = timeit.Timer(ff, "from __main__ import " + func)
         # try:
         #    t = timer.timeit(1)
         # except:
         #    t = timeout_time
         #    timeouts.append(func)
         try:
            t1 = time.time()
            answer = eval(ff)
            t2 = time.time()
            t = t2 - t1
            print(answer, t)
         except:
            t = timeout_time
            timeouts.append(func)
         times[func] = t
         these_times[func] = t
         sys.stdout.flush()   
      these_sorted = sorted(these_times.items(), key=lambda k: k[1])
      print("funcs:", these_sorted)
      best.append(these_sorted[0])
      worst.append(these_sorted[-1])      

   print(sorted(times.items(), key=lambda k: k[1], reverse=True))
   print("timeouts:", timeouts)
   print("best:", [b[0] for b in best])
   print("best total time:", sum(b[1] for b in best))
   print("worst total time:", sum(w[1] for w in worst))

   
if __name__ == '__main__':
   # just run one problem
   if len(sys.argv) > 1:
      if sys.argv[1] == "all":
         run_all_all()
      else:
         func = sys.argv[1]
         eval("bench("+func + ")")
         # eval(func + "()")   

   else:
      # sys.setcheckinterval(100000) # tweak
      run_all()
      print("all (sorted):")
      print(sorted(all_results.items(), key=lambda k: k[1], reverse=True))
      print("errors:", errors)
      print("very_bad (>2s) :", very_bad)
      print("bad (> 1s):", bad)
      print("total_time:", total_time[0])
