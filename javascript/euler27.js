/* 

  Euler #27 in JavaScript.

  Problem 27.
  """
  Euler published the remarkable quadratic formula:

  n^2 + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values 
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which 
  produces 80 primes for the consecutive values n = 0 to 79. The product of the 
  coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

      n^2 + an + b, where |a| < 1000 and |b| < 1000

      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic 
  expression that produces the maximum number of primes for consecutive 
  values of n, starting with n = 0.
  """ 

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {isPrimeCached,timing2} = require('./js_utils.js');

// 45ms
const euler27a = function() {
    const t = 999;
    let bestLen = 0;
    let bestA = 0;
    let bestB = 0;
    for(let a = -t; a <= t; a++) {
        for(let b = -t; b <= t; b++) {
            let len = 0;
            let pp = len**2 + a*len + b;
            while (pp > 1 && isPrimeCached(pp)) {
                len++;
                pp = len**2 + a*len + b;
            }
            if (len > bestLen) {
                bestLen = len;
                bestA = a;
                bestB = b;
               }
        }
    }
    
    return(bestA * bestB);
}


timing2(euler27a); // 45ms
