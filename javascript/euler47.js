/* 

  Euler #47 in JavaScript.

  Problem 47:
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

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 74ms
var euler47a = function() {
    var maxn = 1000000;
    var f = Array(maxn).fill(0);
    for(var i = 2; i < maxn; i++) {
        if (f[i] === 0) {
            for(var j = 2*i; j < maxn; j+=i) {
                f[j]++;
            }
        }
    }
    
    var goal = [4,4,4,4].join();
    var found = 0;
    for(var i = 2; i < maxn-3; i++) {
        // console.log([i,[f[i],f[i+1],f[i+2],f[i+3]]]);
        // Ah, this don't work!
        /*
        if ([f[i],f[i+1],f[i+2],f[i+3]] === goal) {
            found = i;
            console.log("found " + i);
            break;
        }
        */
        // Using join() works...
        if ([f[i],f[i+1],f[i+2],f[i+3]].join() === goal) {
            found = i;
            break;
        }
    }
    return found;
}

timing2(euler47a); // 74ms


