/* 

  Euler #10 in JavaScript.

  Problem 10
  """ 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  
  Find the sum of all the primes below two million.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {primes,isPrime,sieve,timing2} = require('./js_utils.js');


// 279ms
const euler10a = function() {
    return primes(2_000_000).sum2();
}

// 248ms
const euler10b = function() {
    let p = 2;
    for(let i=3; i < 2_000_000; i+=2) {
        if (isPrime(i)) {
            p+=i;            
        }
    }
    return p;
}

// 336ms
const euler10c = function() {
    return sieve(2_000_000).sum2();
}

// timing2(euler10a); // 279ms
timing2(euler10b); // 248ms
// timing2(euler10c); // 336ms
