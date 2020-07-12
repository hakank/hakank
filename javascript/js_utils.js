/*

  Some JS utilities.

  Notations which is mostly followed (there are a few exceptions):
  - <name>: normal function,
    e.g. sum -> sum([1,2,3,4,5])

  - <name>2: chain variant of <name>
    e.g. sum2 -> [1,2,3,4,5].sum2()
    These variant are created via 
       *.prototype.<name>2 = <name>2;

  - <name>N: BigInt variant of <name>
    e.g. range2N : BigInt variant of range2 which is a chain variant of range.

  Exceptions from this naming standard:
   - range2(from,to) is a variant of range(n) (which generates [0..n-1])
   - timing2 is a variant of timing which outputs [time,result]
     wheres timing returns the time.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/
  
*/
'use strict';


// Return time difference (millis)
exports.timing = function(f) {
    var t1_start = +new Date(); // new Date().getTime();
    f();
    var t1_end = +new Date(); // new Date().getTime();
    return t1_end - t1_start;
}

// Prints [time difference, result]
exports.timing2 = function(f) {
    console.log(f.name);
    var t1_start = +new Date();
    var res = f();
    var t1_end = +new Date();
    console.log([t1_end - t1_start, res]);
}

// range(n) -> [0,1,2,3...n-1]
const range = function(n) { return [...Array(n).keys()]; }
exports.range = range;

// range2(from, to) -> [from,from+1,from+2,...,to]
const range2 = function(from, to) {
    var n = to - from +1;
    return [...Array(n).keys()]
        .map(i=>i+from);
    
}
exports.range2 = range2;

// range2(from, to) -> [from,from+1,from+2,...,to]
const range2N = function(from, to) {
    var n = BigInt(to) - BigInt(from) +1n;
    return [...Array(n).keys()]
        .map(i=>BigInt(i)+BigInt(from));
    
}
exports.range2N = range2N;


const flatten = function(a) { return [].concat.apply([],a); }
exports.flatten = flatten;

Array.prototype.flatten2 = function() { return [].concat.apply([],this); }
exports.flatten2 = Array.prototype.flatten2;

// sum a list of integers
/*
const sum = function(array) { return array.reduce((a,b)=>a+b); }
*/
const sum = array => array.length > 0 ? array.reduce((a,b)=>a+b) : 0;
exports.sum = sum;

// sum of a list of BigInt numbers 
const sumN = function(a) { return a.length > 0 ? a.reduce((i,j)=>BigInt(i)+BigInt(j)) : undefined;}
exports.sumN = sumN;


// Chain variant of sum/1.
const sum2 = function() { return this.length > 0 ? this.reduce((a,b)=>a+b) : 0; }
Array.prototype.sum2 = sum2
exports.sum2 = Array.prototype.sum2;

// products of a list of numbers
const prod = function(a) { return a.length > 0 ? a.reduce((i,j)=>i*j) : undefined;}
exports.prod = prod;

// products of a list of BigInt numbers 
const prodN = function(a) { return a.length > 0 ? a.reduce((i,j)=>BigInt(i)*BigInt(j)) : undefined;}
exports.prodN = prodN;

// chain variant of prod2N
Array.prototype.prod2 = function() { return this.length > 0 ? this.reduce((a,b)=>a*b) : undefined; }
exports.prod2 = Array.prototype.prod2;

// chain variant of prod
Array.prototype.prod2N = function() { return this.length > 0 ? this.reduce((a,b)=>BigInt(a)*BigInt(b)) : undefined; }
exports.prod2N = Array.prototype.prod2N;


// Return maximum element in array
exports.max = function(array) {return array.reduce((a,b)=>a>b?a:b);}
// Chain variant of max
Array.prototype.max2 = function() {return this.reduce((a,b)=>a>b?a:b);}
exports.max2 = Array.prototype.max2;

// memo/cache of fun
// From https://scotch.io/tutorials/understanding-memoization-in-javascript
function memoizer(fun){
    let cache = {}
    return function (n){
        if (cache[n] != undefined ) {
          return cache[n]
        } else {
          let result = fun(n)
          cache[n] = result
          return result
        }
    }
}
exports.memoizer = memoizer;


exports.last = function(a) { if (a.length > 0) {return a[a.length-1]; } }

exports.butLast = function(a) { if (a.length > 1) {return a[a.length-2]; }}

// Convert a number to its (integer) digits
const num_to_list = function(n) {
    return n.toString().split("").map(i=>parseInt(i));
}
exports.num_to_list = num_to_list;

// chain version
const num_to_list2 = function() {
    return this.toString().split("").map(i=>parseInt(i));
}
Number.prototype.num_to_list2 = num_to_list2;
exports.num_to_list2 = Number.prototype.num_to_list2;

//
// Return fibonacci(n). Memoized
//
const fib = memoizer(function(n) {
    if (n <= 1) {
        return 1;
    } else {
        return fib(n-1)+fib(n-2);
    }

})
exports.fib = fib;

//
// Return fibonacci(n) for BigInt. Memoized
//
const fibN = memoizer(function(n) {
    if (n <= 1n) {
        return 1n;
    } else {
        return fibN(n-1n)+fibN(n-2n);
    }

})
exports.fibN = fibN;


//
// Returns the prime divisors of n.
// TODO: Make this more efficient.
//
const prime_divisors = function(n) {
    if (n === 1) {
        return [];
    }
    var divisors = [];
    while (n > 1) {
        var m = Math.ceil(Math.sqrt(n));
        for(var i = 2; i <= m; i++) {
            if (n % i == 0 && isPrime(i)) {
                divisors.push(i);
                n /= i;
                continue;
            }
        }
    }    
    return divisors;
}
exports.prime_divisors = prime_divisors;

// prime factors of n
var factors = function(n) {
    if (n === 1) {
        return [1];
    }
    var f = [];
    while (n % 2 === 0) {
        f.push(2);
        n /= 2;
    }
    var t = 3;
    while (n > 1 && t < Math.ceil(Math.sqrt(n))) {
        while (n % t === 0) {
            f.push(t);
            n /= t;
        }
        t += 2
    }
    if (n > 1) {
        f.push(n);
    }
    return f;
}
exports.factors = factors;

// convert a list of elements in a to a hash with 
//   { element: number of occurrences of element, ...}
var collect = function(a) {
    var m = {};
    a.forEach(e=> {
        if (m[e] === undefined) {
            m[e] = 0;
        }
        m[e] += 1;
    })
    return m;
}
exports.collect = collect;

// All divisors of a number, except 1 and n
const all_divisors = function(n) {
    var divisors = [];
    var m = n / 2;
    for(var i = 2; i <= m; i++) {
        if (n % i == 0) {
            divisors.push(i);
        }
    }
    return divisors;
    
}
exports.all_divisors = all_divisors;

// All divisors of n, including 1 and n
const all_divisors2 = function(n) {return [1].concat(all_divisors(n)).concat([n]) };
exports.all_divisors2 = all_divisors2;


// All divisors of n, including 1 (but not n)
const all_divisors3 = function(n) {
    var divisors = [1];
    var m = n / 2;
    for(var i = 2; i <= m; i++) {
        if (n % i == 0) {
            divisors.push(i);
        }
    }
    return divisors;    
}
exports.all_divisors3 = all_divisors3;


// Return all primes <= n
const primes = function(n) {
    var primes = [2];
    for(var i = 3; i <= n ; i+=2) {
        if (isPrime(i)) {
            primes.push(i);
        }
    }
    return primes;
}
exports.primes = primes;

// Find the primes <= n
const sieve = function(n) {
    var a = range(n);
    a[0] = 0;
    a[1] = 0;
    for(var i = 2; i < Math.ceil(Math.sqrt(n)); i++) {
        for(var j =i*i; j <= n; j+=i) {
            a[j] = 0;
        }      
    }
    return range(n).filter(i=>a[i]>0);
    
}
exports.sieve = sieve;

// Returns true if n is a prime.
// TODO: Make this more efficient
const isPrime = function(n) {
    if (n < 2) {
        return false;
    }
    if (n === 2 || n === 3) {
        return true;
    }

    if (n % 2 === 0) {
        return false;
    }

    var m = Math.ceil(Math.sqrt(n));
    for (var i = 3; i <= m; i+=2) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
  
}
exports.isPrime = isPrime;

const isPrimeCached = memoizer(isPrime);
exports.isPrimeCached = isPrimeCached;

// GCD(a,b)
const gcd = function(a, b) { return a == 0 ? b : gcd(b % a, a); }
exports.gcd = gcd;

// LCM(a,b)
const lcm = function(a, b) { return (a*b)/gcd(a,b); }
exports.lcm = lcm;

// Factorial(n)
var factorial = function(n) {  return n === 0 ? 1 : prod(range2(1,n)); }
exports.factorial = factorial;

var factorial2 = function() {  return this === 0 ? 1 : prod(range2(1,this)); }
Number.prototype.factorial2 = factorial2;
exports.factorial2 = Number.prototype.factorial2

// factorial(n) for BigInt
var factorialN = function(n) {  return n === 0n ? 1n : prodN(range2N(BigInt(1),BigInt(n))); }
exports.factorialN = factorialN;

//
// convert a decimal integer to a list of base <base> digits.
//
var dec2base = function(n,base) {
    var res = [];
    while (n > 0) {
        res.unshift(n % base);
        n = Math.floor(n / base);
    }
    return res;
}
exports.dec2base = dec2base;

// return the length of a number (instead of .toString().split("").length)
var nlen = function(n) {
    return Math.floor(Math.log10(n))+1;
}
exports.nlen = nlen;

// Is n a palindromic number?
// Used in euler4.js
const palindromic_number = function(n) {
    var s = n.toString();
    var len = s.length;
    for(var i = 0; i <= len/2; i++) {
        if (s[i] != s[len-i-1]) {
            return false;
        }
    }
    return true;
}
exports.palindromic_number = palindromic_number;

// is the list a palindrom?
const palindromic_list = function(s) {
    var len = s.length;
    for(var i = 0; i <= len/2; i++) {
        if (s[i] != s[len-i-1]) {
            return false;
        }
    }
    return true;
}
exports.palindromic_list = palindromic_list;

var is_pandigital = function(s) {
    return s.length === 9 && !s.match(/0/) && new Set(s).size === 9;
}
exports.is_pandigital = is_pandigital;

// Transpose of a matrix
const transpose = function(m) {
    return m.map((_, col) => m.map(row => row[col]));
}
exports.transpose = transpose;


// All diagonals of a matrix
var all_diagonals = function(m) {
    var n = m.length;
    var num_diagonals = (n*2);
    var diagonals = [];
    for (var k = 0; k < num_diagonals; k++) {
        var d1 = [];
        var d2 = [];        
        for(var i = 0; i < n; i++) {
            for(var j = 0; j < n; j++) {
                if (i+j === k) {
                    d1.push(m[i][j])
                }
                if (i+(n-j) === k) {
                    d2.push(m[j][i])
                }
            }
        }
        if (d1.length > 0) {
            diagonals.push(d1);
        }
        if (d2.length > 0) {
            diagonals.push(d2);
        }
    }
    
    return diagonals;
}
exports.all_diagonals = all_diagonals;


//
// Next permutation of array p
//
var next_permutation = function(p) {
    var i = p.length - 1;
    while (i > 0 && p[i-1] >= p[i])
        i--;
    if (i <= 0) {
        return null;
    }
    
    var j = p.length - 1;
    while (p[j] <= p[i - 1]) {
        j--;
    }
    
    // swap
    var t = p[i - 1];
    p[i - 1] = p[j];
    p[j] = t;
    
    j = p.length - 1;
    while (i < j) {
        // swap
        t = p[i];
        p[i] = p[j];
        p[j] = t;
        i++;
        j--;
    }
    
    return p;
}

exports.next_permutation = next_permutation;


// Inspired by an answer from
// http://stackoverflow.com/questions/4240080/generating-all-permutations-of-a-given-string
var all_permutations = function(s) {
    var perms = [];
    permutation_tmp([], s, perms);
    return perms;
}
var permutation_tmp = function(prefix,s, perms) {
    var n = s.length;
    if (n == 0) {
        perms.push(prefix);
    } else {
        for (var i = 0; i < n; i++)
            permutation_tmp(prefix.concat(s[i]), s.slice(0, i).concat(s.slice(i+1, n)), perms);
    }
}

exports.all_permutations = all_permutations;
