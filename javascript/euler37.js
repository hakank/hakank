/* 

  Euler #37 in JavaScript.

  Problem 37:
  """
  The number 3797 has an interesting property. Being prime itself, it is possible to 
  continuously remove digits from left to right, and remain prime at each stage: 
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

  Find the sum of the only eleven primes that are both truncatable from left to right 
  and right to left.

  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {nlen,isPrime,timing2} = require('./js_utils.js');

// check is n is a truncated prime
var check = function(n) {
    var len = nlen(n);
    // var len = n.toString().split("").length; // slower as expected (68ms)
    for(var i = 1; i < len; i++) {
        var ii = 10**i;
        if (!isPrime(n % ii) || !isPrime(Math.floor(n / ii))) {
            return false;
        }
    }
    return true;
}

// 31ms
var euler37a = function() {
    var p = 11;
    var sum = 0;
    var c = 0;
    while (c < 11) {
        if (check(p) && isPrime(p)) {
            c++;
            sum += p
        }
        p+=2;
    }
    return sum;
}

timing2(euler37a); // 31ms


