/* 

  de Bruijn sequences in JavaScript.

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
  
  This is a port of the Java version
  http://www.hakank.org/comb/deBruijn.java
  or rather a port of the original C program with
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

  Compare with my web based programs:
  - http://www.hakank.org/comb/debruijn.cgi   
  - http://www.hakank.org/comb/deBruijnApplet.html


  

  Syntax: 
    $ node debruijn.js <k> <n> <p>
      where
       n: the language: 0..n-1
       k: length of each sub sequence
       p: if a flatten version should be printed as well

  Examples:

    $ node debruijn.js 2 3 1
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
    $ node debruijn.js 10 4 1
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
    7 8 9 8 7 8 9 9 7 9 7 9 8 8 7 9 8 9 7 9 9 8 7 9 9 9 8 8 8 8 9 8 8 9 9 8 9 8 9 9 9 9 (0 0 0)
    L.length: 10000
    length: 10**4+4-1 = 10000 + 3 = 10003

    (See http://www.hakank.org/comb/debruijn_k_10_n_4.html )


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range} = require('./js_utils.js');

let a = [];
let n = 3;
let k = 2;
let L = [];

function pp(s) {
    process.stdout.write(s.toString());
}

function main(args) {

    // the door lock combination solution
    // is k = 10, n = 4
    // Also see: http://www.hakank.org/comb/debruijn_k_10_n_4.html

    // k: language (0..k-1)
    k = args.length>0 ? args[0] : 2; // 10   

    // n: string length
    n = args.length>1 ? args[1] : 3; // 4

    // also print the sequence as a flat list?
    let print_seq = args[2] || 1;

    console.log(`k:${k} n: ${n} i.e. language: 0..k-1 (0..${k-1}) and with string len n (${n})`);

    L = [];
    a = new Array(101).fill(0); // range(101);
    a[0] = 0;

    Gen(1,1,0);
    
    // print as a flat sequence
    if (print_seq > 0) {
        pp("\nAs a flat sequence: ")
        pp(L.join(" "));
        // and wrap the first n-1 items
        pp(" (");
        pp(L.slice(0,n-1).join(" "));
        pp(")");
        console.log();
        console.log("L.length:",L.length);
    }

    // console.log("length: ", k,"**", n,"+",n, "-1 = ",  k**n, " + ", n-1, " = ", k**n+n-1 )
    console.log(`length: k**n+n-1 = ${k**n} + ${n-1} = ${k**n+n-1}`);

}


//
// When to "print" a digit
//
function Print(p) {
    if (n % p === 0) {
        for(let j = 1; j <= p; j++) {
            let t = a[j];
            L.push(t);
            pp(t);
            pp(" ");
        }
        console.log();
    }

}


function Gen(t, p, ones) {
    // console.log(`Gen(${t}, ${p}, ${ones})`);
    if (ones <= n) {
        if (t > n) {
            Print(p);
        } else {
            a[t] = a[t-p];
            if (a[t] > 0) {
                Gen(t+1,p,ones+1);
            } else {
                Gen(t+1,p,ones)
            }
            for(let j = a[t-p]+1; j <= k-1; j++) {
                a[t] = j;
                Gen(t+1,t,ones+1);
            }
        }
    }
    
}



const args = process.argv.slice(2).map(n=>parseInt(n));
console.log("args:",args);

// main([2,3,1]); // k = 2, n=3, and print the flat sequence
// main([10,4,1]); // k = 10, n=4, and print the flat sequence. The port code sequence.

main(args);
