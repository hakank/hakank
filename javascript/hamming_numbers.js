/* 

  Hamming Numbers in JavaScript.

  http://rosettacode.org/wiki/Hamming_numbers
  """ 
  Hamming numbers are numbers of the form

  H = 2^i . 3^j . 5^k where} i, j, k > 0. 

  Generate the sequence of Hamming numbers, in increasing order. In particular:

     1. Show the first twenty Hamming numbers.
     2. Show the 1691st Hamming number (the last one below 2^31).
     3. Show the one millionth Hamming number (if the language - 
        a convenient library -upports arbitrary-precision integers). 
   
   ...

   The result should be:
   1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 
   2125764000
   519312780448388736089589843750000000000000000000000000000000000000000000000000000000
   """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {min2N,range,range2,timing2} = require('./js_utils.js');

// This is much slower than min2N (from js_utils.js)
function minN(a) {
    let minVal = a[0];
    for (const i of a) {
        if (a[i] < minVal) {
            minVal = a[i];
        }
    }
    return minVal;
}


// hamming for Small Int
function hamming(n) {
    let q = new Set([1,2,3,5]);
    let low = 1;
    for (let i of range(n)) {
        low = Math.min(...q.values())
        q.delete(low);
        [low*2,low*3,low*5].map(v=>q.add(v));
    }
    return low;
}

// hamming for Big Int: very slow for 1_000_000)
function hammingN(n) {
    let q = new Set([1n,2n,3n,5n]);
    let low = 1n;
    for (let i of range(n)) {
        low = [...q.values()].minN();
        q.delete(low);
        [low*2n,low*3n,low*5n].map(v=>q.add(v));
    }
    return low;
}

// hamming2: faster
function hamming2(n) {

    let h = [1];
    let [next2,next3,next5] = [2,3,5];
    let i = 0, j = 0, k = 0; // counters
    let m = 1;
    while (m < n) {
        h[m] = Math.min(...[next2,next3,next5]);        
        // Note that we might update more than one counters
        if (h[m] === next2) { i++; next2 = 2*h[i]; }
        if (h[m] === next3) { j++; next3 = 3*h[j]; }
        if (h[m] === next5) { k++; next5 = 5*h[k]; }
        m++;
    }
    
    return h[n-1];

}

// hamming2 for BigInt: much faster
function hamming2N(n) {

    let h = [1n];
    let [next2,next3,next5] = [2n,3n,5n];
    let i = 0n, j = 0n, k = 0n; // counters
    let m = 1n;
    while (m < n) {
        h[m] = [next2,next3,next5].min2N();
        // Note that we might update more than one counters
        if (h[m] === next2) { i++; next2 = 2n*h[i]; }
        if (h[m] === next3) { j++; next3 = 3n*h[j]; }
        if (h[m] === next5) { k++; next5 = 5n*h[k]; }
        m++;
    }
   
    return h[n-1];
}


console.log("hamming(1..20):", range2(1,20).map(n=>hamming(n)));
console.log("hamming(1691):", hamming(1691));
// console.log("hamming(1_000_000):", hammingN(1_000_000)); // Too slow

console.log("hamming2(1..20):", range2(1,20).map(n=>hamming2(n)));
console.log("hamming2(1691):", hamming2(1691));

// Much faster than hammingN
console.log("hamming2N(1_000_000):", timing2(()=> hamming2N(1_000_000)));



