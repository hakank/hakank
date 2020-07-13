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


// 272ms
const euler10a = function() {
    return primes(2000000).sum2();
}

// 266ms
const euler10b = function() {
    let p = 2;
    for(let i=3; i < 2000000; i+=2) {
        if (isPrime(i)) {
            p+=i;            
        }
    }
    return p;
}

// 205ms: This is the fastest!
const euler10c = function() {
    return sieve(2000000).sum2();
}

timing2(euler10a);
// timing2(euler10b);
// timing2(euler10c);
