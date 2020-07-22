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
  See also my JavaScript page: http://www.hakank.org/javascript_progs/
  
*/
'use strict';


// Return time difference (millis)
exports.timing = function(f) {
    const t1_start = +new Date(); // new Date().getTime();
    f();
    const t1_end = +new Date(); // new Date().getTime();
    return t1_end - t1_start;
}

// Prints [time difference, result]
exports.timing2 = function(f) {
    console.log(f.name);
    const t1_start = +new Date();
    const res = f();
    const t1_end = +new Date();
    console.log([t1_end - t1_start, res]);
}

// range(n) -> [0,1,2,3...n-1]
const range = function(n) { return [...Array(n).keys()]; }
exports.range = range;

// range2(from, to) -> [from,from+1,from+2,...,to]
const range2 = function(from, to) {
    const n = to - from +1;
    return [...Array(n).keys()]
        .map(i=>i+from);
    
}
exports.range2 = range2;

// range2(from, to) -> [from,from+1,from+2,...,to]
const range2N = function(from, to) {
    const n = BigInt(to) - BigInt(from) +1n;
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


// Return minimum element in array
exports.min = function(array) {return array.reduce((a,b)=>a<b?a:b);}
// Chain variant of min
Array.prototype.min2 = function() {return this.reduce((a,b)=>a<b?a:b);}
exports.min2 = Array.prototype.min2;

// Return minimum element in array for BigInt
Array.prototype.min2N = function() { return this.reduce((a,b)=>{const aa=BigInt(a); const bb=BigInt(b);
                                                               return aa < bb ? aa : bb;
                                                              })}
exports.min2N = Array.prototype.min2N;

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
    let divisors = [];
    while (n > 1) {
        const m = Math.ceil(Math.sqrt(n));
        for(let i = 2; i <= m; i++) {
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
const factors = function(n) {
    if (n === 1) {
        return [1];
    }
    let f = [];
    while (n % 2 === 0) {
        f.push(2);
        n /= 2;
    }
    let t = 3;
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
const collect = function(a) {
    let m = {};
    a.forEach(e=> {
        if (m[e] === undefined) {
            m[e] = 0;
        }
        m[e] += 1;
    })
    return m;
}
exports.collect = collect;


// convert a list of elements in a to a hash with 
//   { element: number of occurrences of element, ...}
// Chain version
const collect2 = function() {
    let m = {};
    this.forEach(e=> {
        if (m[e] === undefined) {
            m[e] = 0;
        }
        m[e] += 1;
    })
    return m;
}
Array.prototype.collect2 = collect2;
exports.collect2 = Array.prototype.collect2;


// All divisors of a number, except 1 and n
const all_divisors = function(n) {
    let divisors = [];
    const m = n / 2;
    for(let i = 2; i <= m; i++) {
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
    let divisors = [1];
    const m = n / 2;
    for(let i = 2; i <= m; i++) {
        if (n % i == 0) {
            divisors.push(i);
        }
    }
    return divisors;    
}
exports.all_divisors3 = all_divisors3;


// Return all primes <= n
const primes = function(n) {
    let primes = [2];
    for(let i = 3; i <= n ; i+=2) {
        if (isPrime(i)) {
            primes.push(i);
        }
    }
    return primes;
}
exports.primes = primes;

// Find the primes <= n
const sieve = function(n) {
    let a = range(n);
    a[0] = 0;
    a[1] = 0;
    for(let i = 2; i < Math.ceil(Math.sqrt(n)); i++) {
        for(let j =i*i; j <= n; j+=i) {
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

    const m = Math.ceil(Math.sqrt(n));
    for (let i = 3; i <= m; i+=2) {
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
const factorial = function(n) {  return n === 0 ? 1 : prod(range2(1,n)); }
exports.factorial = factorial;

const factorial2 = function() {  return this === 0 ? 1 : prod(range2(1,this)); }
Number.prototype.factorial2 = factorial2;
exports.factorial2 = Number.prototype.factorial2

// factorial(n) for BigInt
// const factorialN = function(n) {  return n === 0n ? 1n : prodN(range2N(BigInt(1),BigInt(n))); }
const factorialN = function(n) { return prodN(range2(1,n)); }
exports.factorialN = factorialN;


// Subfactorial
function subfactorial(n) {
    if (n <= 0) {
        return 1;
    } else if (n === 1) {
        return 0;
    } else {
        return (n-1)*(subfactorial(n-1) + subfactorial(n-2));
    }
}
exports.subfactorial = subfactorial;

// Subfactorial, BigInt version
function subfactorialN(n) {
    if (n <= 0n) {
        return 1n;
    } else if (n === 1n) {
        return 0n;
    } else {
        return (n-1n)*(subfactorialN(n-1n) + subfactorialN(n-2n));
    }
}
exports.subfactorialN = subfactorialN;

// Subfactorial, BigInt version, cached
const subfactorialNCached = memoizer(function(n) {
    if (n === 0n) {
        return 1n;
    } else if (n === 1n) {
        return 0n;
    } else {
        return (n-1n)*(subfactorialNCached(n-1n)+subfactorialNCached(n-2n));
    }
    return undefined;
})
exports.subfactorialNCached = subfactorialNCached;

//
// convert a decimal integer to a list of base <base> digits.
//
const dec2base = function(n,base) {
    let res = [];
    while (n > 0) {
        res.unshift(n % base);
        n = Math.floor(n / base);
    }
    return res;
}
exports.dec2base = dec2base;

// return the length of a number (instead of .toString().split("").length)
const nlen = function(n) {
    return Math.floor(Math.log10(n))+1;
}
exports.nlen = nlen;

// Is n a palindromic number?
// Used in euler4.js
const palindromic_number = function(n) {
    const s = n.toString();
    const len = s.length;
    for(let i = 0; i <= len/2; i++) {
        if (s[i] != s[len-i-1]) {
            return false;
        }
    }
    return true;
}
exports.palindromic_number = palindromic_number;

// is the list a palindrom?
const palindromic_list = function(s) {
    const len = s.length;
    for(let i = 0; i <= len/2; i++) {
        if (s[i] != s[len-i-1]) {
            return false;
        }
    }
    return true;
}
exports.palindromic_list = palindromic_list;

const is_pandigital = function(s) {
    return s.length === 9 && !s.match(/0/) && new Set(s).size === 9;
}
exports.is_pandigital = is_pandigital;

/*
// Transpose of a matrix
// This version is pretty require that it's a square matrix
const transpose = function(m) {
    return m.map((_, col) => m.map(row => row[col]));
}
exports.transpose = transpose;

// Transpose of a matrix, chain version
const transpose2 = function() {
    return transpose(this);
}
*/
//
// This version handles both square and non square matrices
// (and is also aliased to zip, zip2)
function transpose(m) {
    const rows = m.length;
    const cols = m[0].length;
    const m2 = new Array(cols);    
    for(let j = 0; j < cols; j++) {
        m2[j] = new Array(rows);
        for(let i = 0; i < rows; i++) {
            m2[j][i] = m[i][j];
        }
    }
    return m2;
}
exports.transpose = transpose;

// Chain version
function transpose2() {
    return transpose(this);
}
Array.prototype.transpose2 = transpose2;
exports.transpose2 = Array.prototype.transpose2;


// All diagonals of a matrix
const all_diagonals = function(m) {
    const n = m.length;
    const num_diagonals = (n*2);
    let diagonals = [];
    for (let k = 0; k < num_diagonals; k++) {
        let d1 = [];
        let d2 = [];        
        for(let i = 0; i < n; i++) {
            for(let j = 0; j < n; j++) {
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
const next_permutation = function(p) {
    let i = p.length - 1;
    while (i > 0 && p[i-1] >= p[i])
        i--;
    if (i <= 0) {
        return null;
    }
    
    let j = p.length - 1;
    while (p[j] <= p[i - 1]) {
        j--;
    }
    
    // swap
    let t = p[i - 1];
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
const all_permutations = function(s) {
    let perms = [];
    permutation_tmp([], s, perms);
    return perms;
}
const permutation_tmp = function(prefix,s, perms) {
    const n = s.length;
    if (n == 0) {
        perms.push(prefix);
    } else {
        for (var i = 0; i < n; i++)
            permutation_tmp(prefix.concat(s[i]), s.slice(0, i).concat(s.slice(i+1, n)), perms);
    }
}

exports.all_permutations = all_permutations;


//
// partitions array a according to the result of
// applying f to each element in a,
// Returns a hash.
// 
function partition(a,f) {
    let part = {};
    for(const e of a) {
        const r = f(e);
        if (part[r] === undefined) {
            part[r] = [];
        }
        part[r].push(e);
    }
    
    return part;
}
exports.partition = partition;

// Chain version of partition/2
function partition2(f) {
    let part = {};
    for(const e of this) {
        const r = f(e);
        if (part[r] === undefined) {
            part[r] = [];
        }
        part[r].push(e);
    }
    
    return part;
}

Array.prototype.partition2 = partition2;
exports.partition2 = partition2;



//
// ensure that all the elements in L are distinct
//
function all_different(L) {
    const len = L.length;

    for(let i = 0; i < len; i++) {
        for(let j = 0; j < i; j++) {
            if (L[i] === L[j]) {
                return false;
            }
        }
        
    }

    return true;
}

exports.all_different = all_different;


//
// This is the odometer function: i.e.
// possible ordered combinations of x with replacements.
// If x in an integer, it will be interpreted as the range 0..n-1.
// 
function odometer(k,x) {
    if (typeof x === 'number') {
        return odometer(k,range(x));
    }    
    if (k===0) {
        return [[]];
    } else if (x === [] || x.length === 0) {
        return [];
    } else {
        const [first,...rest] = x;
        const t = odometer(k-1,x).map(c=>[first].concat(c)); 
        return t.concat(odometer(k,rest));
    }
    return undefined;
}
exports.odometer = odometer;
exports.combinations_with_replacements = odometer;


//
// All k combinations of elements in x without replacements
// (or 0..n-1 if x in an integer).
//
function combinations(k,x) {
    if (typeof x === 'number') {
        return combinations(k,range(x));
    }
    if (k===0) {
        return [[]];
    } else if (x === [] || x.length === 0) {
        return [];
    } else {
        const [y,...ys] = x;
        const t = combinations(k-1,ys).map(c=>[y].concat(c)); 
        return t.concat(combinations(k,ys));
    }
    return undefined;
}

exports.combinations = combinations;


//
// find elements that are in a but not in b
//
function difference(a,b) {
    let ahash = new Set(a);
    let bhash = new Set(b);

    let diffs = [];
    for(let e of ahash.keys()) {
        if (!bhash.has(e)) {
            diffs.push(e);
        }
    }
    return diffs;
}
exports.difference = difference;


// Sort function for numbers
function numsortf(a,b) {
    a - b;
}
exports.numsortf = numsortf;

//
// return array a without element e
//
function exclude_array(a,e) {
    return a.filter(i=>i!==e);
}
exports.exclude_array = exclude_array;

// returns the powerset of array s
function powerset_array(a) {
    if (a === [] || a.length === 0) {
        return [[]];
    } else {
        for(let x of a) {
            let a2 = exclude_array(a,x);
            let q = powerset_array(a2);
            return q.concat(q.map(y=>[x].concat(y)));
        }
    }
    
    return undefined;
}
exports.powerset_array = powerset_array;



// zip 2 arrays
/*
const zip = (a1, a2) => a1.map((k, i) => [k, a2[i]]);
exports.zip = zip;

const zip2 = () => this[0].map((k, i) => [k, this[1][i]]);
Array.prototype.zip2 = zip2;
exports.zip2 = zip2;
*/
/*
function zip(a) {
    // We have to remove training empty arrays.
    // (Though this suggest that this approach might be a bit incorrect)
    return a.map((k,i)=>range(len).filter(t=>i<a0len).map(j=>a[j][i])).skip_empty2();

}
*/
// Well, it's easier to alias to transpose!
exports.zip = transpose;

// chain variant
Array.prototype.zip2 = transpose2;
exports.zip2 = Array.prototype.zip2;

// Remove undefined in a matri / list
function skip_undefined(a) {
    if (a[0].length !== undefined) {
        return a.map(c=>c.filter(i=>i !== undefined));
    } else {
        return a.filter(i=>i !== undefined);
    }
}
exports.skip_undefined = skip_undefined;

// Chain version
function skip_undefined2() {
    return skip_undefined(this);
}
Array.prototype.skip_undefined2 = skip_undefined2;
exports.skip_undefined2 = skip_undefined2;

// Remove empty lists
function skip_empty(a) {
    return a.filter(i=>i.length > 0);    
}

function skip_empty2() {
    return skip_empty(this);
}
Array.prototype.skip_empty2 = skip_empty2;

// Return a random integer from 0..max
function randomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}
exports.randomInt = randomInt;

//
// Return an array of n with random integers
//
function randomIntArray(n,max) {
    return range(n).map(i=>randomInt(max));
}
exports.randomIntArray = randomIntArray;

//
// Generates random integers in the range -max .. max
//
function randomIntArrayNeg(n,max) {
    return range(n).map(i=>randomInt(2*max)-max);
}
exports.randomIntArrayNeg = randomIntArrayNeg;


//
// Remove duplicates from a list
//
function remove_duplicates(a) {
    let h = {};
    let res = [];
    for(let e of a) {
        if (!h[e]) {
            res.push(e);
            h[e] = 1;
        }
    }
    return res;
}
exports.remove_duplicates = remove_duplicates;

// Chain version
function remove_duplicates2() {
    return remove_duplicates(this);
}
Array.prototype.remove_duplicates2 = remove_duplicates2;
exports.remove_duplicates2 = Array.prototype.remove_duplicates2;

// Dot product of a and b
function dot_product(a,b) {
    let dot = 0;
    for(let i=0; i < a.length; i++) {
        dot += a[i]*b[i];
    }
    return dot;
}
exports.dot_product = dot_product;

// binomial (n over k)
function binomial(n,k) { 
    if (k < 0 || k > n) {
        return 0;
    }   
    return factorial(n)/(factorial(n-k)*factorial(k));
}
exports.binomial = binomial;

// binomial (n over k) BigInt
function binomialN(n,k) {
    if (k < 0 || k > n) {
        return 0;
    }
    return factorialN(n)/(factorialN(n-k)*factorialN(k));
}
exports.binomialN = binomialN;


function gray_encode(n) {
    return n ^ (n >> 1);
}
exports.gray_encode = gray_encode;

function gray_decode(n) {
    let p = n;
    n = n >> 1;
    while (n != 0) {
        p = p ^ n;
        n = n >> 1;
    }
    return p;
}
exports.gray_decode = gray_decode;



/*
function asum(a) {
    let c = 0;
    let res = [];
    for(let i = 0; i < a.length; i++) {
        c+=a[i];
        res.push(c);
    }
    return res;
}
*/
//
// Accumulated sum
//
function asum(a) {
    return scan(a,(i,j)=>i+j);
}
exports.asum = asum;


function asum2() {
    return asum(this);
}
Array.prototype.asum2 = asum2;
exports.asum2 = Array.prototype.asum2

//
// scan(a, function)
// Returns the accumulated operations of f on array a
//
function scan(a,f) {
    let c = 0;
    let res = [];
    for(let i = 0; i < a.length; i++) {
        c = f(c,a[i]);
        res.push(c);
    }
    return res;
}
exports.scan = scan;

function scan2(f) {
    return scan(this,f);
}
Array.prototype.scan2 = scan2;
exports.scan2 = Array.prototype.scan2;

//
// Single value argmax: index of the largest value in a
//
function argmax(a) {
    let ix = 0;
    let imax = a[0];
    for(let i = 1; i < a.length; i++) {
        if (a[i] > imax) {
            imax = a[i];
            ix = i;
        }
    }
    return ix;
}
exports.argmax = argmax;

function argmax2() {
    return argmax(this);
}
Array.prototype.argmax2 = argmax2;
exports.argmax2 = Array.prototype.argmax2;

//
// Single value argmin: index of the smallest value in a
//
function argmin(a) {
    let ix = 0;
    let imin = a[0];
    for(let i = 1; i < a.length; i++) {
        if (a[i] < imin) {
            imin = a[i];
            ix = i;
        }
    }
    return ix;
}
exports.argmin = argmin;

function argmin2() {
    return argmin(this);
}
Array.prototype.argmin2 = argmin2;
exports.argmin2 = Array.prototype.argmin2;


//
// Multiple value version of argmax
//
function argmaxes(a) {
    let ixs = [];
    let imax = a[0];
    for(let i = 1; i < a.length; i++) {
        if (a[i] > imax) {
            imax = a[i];
            ixs = [i];
        } else if (a[i] === imax) {
            ixs.push(i);
        }
    }
    return ixs;
}
exports.argmaxes = argmaxes;

function argmaxes2() {
    return argmaxes(this);
}
Array.prototype.argmaxes2 = argmaxes2;
exports.argmaxes2 = Array.prototype.argmaxes2;


//
// Multiple value version of argmin
//
function argmins(a) {
    let ixs = [];
    let imin = a[0];
    for(let i = 1; i < a.length; i++) {
        if (a[i] < imin) {
            imin = a[i];
            ixs = [i];
        } else if (a[i] === imin) {
            ixs.push(i);
        }
    }
    return ixs;
}
exports.argmins = argmins;

function argmins2() {
    return argmins(this);
}
Array.prototype.argmins2 = argmins2;
exports.argmins2 = Array.prototype.argmins2;

//
// Return a random element from the array a
function random_element(a) {
    return a[randomInt(a.length)];
}
exports.random_element = random_element;

function random_element2() {
    return random_element(this);
}
Array.prototype.random_element2 = random_element2;
exports.random_element2 = Array.prototype.random_element2;


//
// return a shuffled version of array a
//
function shuffle(a) {
    let b = [...a];
    const len = a.length;
    for (let i = 0; i < len*2; i++) {
        const r = randomInt(len);
        [b[i],b[r]] = [b[r],b[i]];
    }

    return b;
}
exports.shuffle = shuffle;

// chain version of shuffle
function shuffle2() {
    return shuffle(this);
}
Array.prototype.shuffle2 = shuffle2;
exports.shuffle2 = Array.prototype.shuffle2;


//
// Create an empty matrix of dimension r x c
// With optional initial values (default=0);
//
function create_matrix(r,c,init=0) {
    let m = new Array(r);
    for(let i = 0; i < r; i++) {
        m[i] = new Array(c).fill(init);
    }
    return m;
}
exports.create_matrix = create_matrix;

//
// Print matrix
//
function print_matrix(m) {
    m.map(row=>console.log(JSON.stringify(row)));
}
exports.print_matrix = print_matrix;

// Chain version
function print_matrix2() {
    print_matrix(this);
}
Array.prototype.print_matrix2 = print_matrix2,
exports.print_matrix2 = print_matrix2


//
// Return a random matrix of dimension r x c with
// max value max.
//
function random_matrix(r,c,max) {
    let m = create_matrix(r,c);
    for(let i = 0; i < r; i++) {
        for(let j = 0; j < c; j++) {
            m[i][j] = randomInt(max);
        }
    }
    return m;
}
exports.random_matrix = random_matrix;

//
// Multiply two matrices (a[m][n] x b[n][p])
// TODO: make it functional
//
function matrix_mult(a,b) {
    const n = a[0].length;
    const m = a.length;
    const p = b[0].length;
    const q = b.length;
    if (n !== q) {
        throw(Error(`Invalid dimensions: ${n} != ${q}`));
    } else {
        let ans = create_matrix(m,p,0);
        for(let i = 0;i < m;i++){
            for(let j = 0;j < p;j++){
                for(let k = 0;k < n;k++){
                    ans[i][j] += a[i][k]*b[k][j];
                }
            }
        }
        return ans;
    }
    
    return undefined;
}
exports.matrix_mult = matrix_mult;

// Chain version
function matrix_mult2(b) {
    return matrix_mult(this,b);
}
Array.prototype.matrix_mult2 = matrix_mult2;
exports.matrix_mult2 = matrix_mult2;


//
// return a matrix where all elements are
// adjusted according to function f
//
function matrix_element_op(m, f) {
    if (m[0].length === undefined) {
        return m.map(e=>f(e));
    } else {
        return m.map(row=>row.map(e=>f(e)));
    }
}
exports.matrix_lement_op = matrix_element_op;

function matrix_element_op2(f) {
    return matrix_element_op(this,f);
}
Array.prototype.matrix_element_op2 = matrix_element_op2;
exports.matrix_lement_op2 = matrix_element_op2;



//
// Scalar operations on two matrices, m1+m2, etc.
// TODO: make it functional.
//
function matrix_matrix_element_op(a,b, f) {
    const n = a[0].length;
    const m = a.length;
    const p = b[0].length;
    const q = b.length;
    console.log("n:",n,"m:",m,"p:",p,"q:",q);
    if (n !== p || m !== q ) {
        throw(Error(`Matrices are not of same dimensions`));
    } else {
        let ret = create_matrix(m,n,0);
        for(let i = 0;i < m;i++){
            for(let j = 0;j < n;j++){
                ret[i][j] = f(a[i][j], b[i][j]);
            }
        }
        return ret;
    }
    
    return undefined;
    
}
exports.matrix_matrix_element_op = matrix_matrix_element_op;

function matrix_matrix_element_op2(b,f) {
    return matrix_matrix_element_op(this,b,f);
}
Array.prototype.matrix_matrix_element_op2 = matrix_matrix_element_op2;
exports.matrix_matrix_element_op2 = matrix_matrix_element_op2;
