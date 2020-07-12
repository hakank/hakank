/* 

  Euler #50 in JavaScript.

  Problem 50:
  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime 
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime, 
  contains 21 terms, and is equal to 953.
  
  Which prime, below one-million, can be written as the sum of the most 
  consecutive primes?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {primes,isPrime,sum2,range2,timing2} = require('./js_utils.js');

// 70ms
var euler50a = function() {
    var n = 10000;
    var p = primes(n);
    var found = false;
    for(var len = 550; len>=21; len--) {
        for(var offset = 1; offset <= 549; offset++) {
            var pp = range2(offset+1,offset+len).map(i=>p[i]).sum2();
            if (pp < 1000000 && isPrime(pp)) {
                return pp;
            }
        }
    }
        
    return undefined;
}

timing2(euler50a); // 70ms

