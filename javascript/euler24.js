/* 

  Euler #24 in JavaScript.

  Euler 24:
  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """ 


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {next_permutation,all_permutations,range,range2,factorial,timing2} = require('./js_utils.js');

// 11ms
const euler24a = function() {
    let a = range(10);
    let c = 1;
    while (c++ < 1000000) {
        a = next_permutation(a);
    }

    return parseInt(a.join(""));
}

// 5023ms (5s)
const euler24b = function() {
    return parseInt(all_permutations(range(10))[1000000-1].join(""));
}

// Inspired by a solution on the 'net
// 0ms
const euler24c = function() {
    let n = 999999;
    let p = 10;
    let eli = range2(1,p).map(i=>i % 10);
    let answer = [];
    for(let i = 1; i < p; i++) {
        let f = factorial(p-i);
        let d = Math.floor(n / f);
        n %= f;
        answer.push(eli[d-1]);
        // Note the indexOf()...
        eli.splice(eli.indexOf(eli[d-1]),1);        
    }
    return parseInt(answer.concat(eli).join(""));
}

// timing2(euler24a);
// timing2(euler24b); // too slow
timing2(euler24c);

