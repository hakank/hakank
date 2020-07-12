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
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {next_permutation,all_permutations,range,range2,factorial,timing2} = require('./js_utils.js');

// 11ms
var euler24a = function() {
    var a = range(10);
    var c = 1;
    while (c++ < 1000000) {
        a = next_permutation(a);
    }

    return a.join("");
}

// 5023ms (5s)
var euler24b = function() {
    return all_permutations(range(10))[1000000-1].join("");
}

// Inspired by a solution on the 'net
// 0ms
var euler24c = function() {
    var n = 999999;
    var p = 10;
    var eli = range2(1,p).map(i=>i % 10);
    var answer = [];
    for(var i = 1; i < p; i++) {
        var f = factorial(p-i);
        var d = Math.floor(n / f);
        n %= f;
        answer.push(eli[d-1]);
        // Note the indexOf()...
        eli.splice(eli.indexOf(eli[d-1]),1);        
    }
    return answer.concat(eli).join("");
}

// timing2(euler24a);
// timing2(euler24b); // too slow
timing2(euler24c);

