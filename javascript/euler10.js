/* 

  Euler #10 in JavaScript.

  Problem 10
  """ 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
  Find the sum of all the primes below two million.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {primes,isPrime,sieve,timing2} = require('./js_utils.js');


// 272ms
var euler10a = function() {
    return primes(2000000).sum2();
}

// 266ms
var euler10b = function() {
    var p = 2;
    for(var i=3; i < 2000000; i+=2) {
        if (isPrime(i)) {
            p+=i;            
        }
    }
    return p;
}

// 205ms: This is the fastest!
var euler10c = function() {
    return sieve(2000000).sum2();
}

// timing2(euler10a);
// timing2(euler10b);
timing2(euler10c);
