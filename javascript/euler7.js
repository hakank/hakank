/* 

  Euler #7 in JavaScript.

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {isPrime,primes,timing2} = require('./js_utils.js');

// 9ms
const euler7a = function() {
    let i = 11;
    let c = 4;
    while (c !== 10001) {
        if (isPrime(i)) {
            c++;
        }
        i += 2;
    }
    return i-2;
}

// 12ms
const euler7b = function() {
    const p = primes(200000); // slightly cheating...
    return p[10000];
}


timing2(euler7a);
// timing2(euler7b);
