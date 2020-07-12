/* 

  Euler #7 in JavaScript.

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
  that the 6^(th) prime is 13.

  What is the 10001^(st) prime number?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {isPrime,primes,last,timing2} = require('./js_utils.js');

// 9ms
var euler7a = function() {
    var i = 11;
    var c = 4;
    while (c !== 10001) {
        if (isPrime(i)) {
            c++;
        }
        i += 2;
    }
    return i-2;
}

// 12ms
var euler7b = function() {
    var p = primes(200000); // slightly cheating...
    return p[10000];
}


timing2(euler7a);
// timing2(euler7b);
