/* 

  Euler #46 in JavaScript.

  Problem 46:
  """  
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {isPrime,timing2} = require('./js_utils.js');

// 5ms
var euler46a = function() {
    var res = 0;
    var gotit = false;
    for(var i = 3; i <= 10000; i+=2) {
        if (!isPrime(i) && !gotit) {
            var s = Math.round(Math.sqrt(i/2));
            var found = 0;
            for(var j = 1; j <= s; j++) {
                if (found === 0) {
                    var ts = j*j*2;
                    if (isPrime(Math.abs(i-ts))) {
                        found = 1;
                    }
                }
            }
            if (found === 0) {
                res = i;
                gotit = true;
                break;
            }
        }
    }
    return res;
}


timing2(euler46a); // 5ms

