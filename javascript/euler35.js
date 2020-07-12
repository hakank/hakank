/* 

  Euler #35 in JavaScript.

  Problem 35
  """
  The number, 197, is called a circular prime because all rotations 
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,num_to_list2,isPrime,isPrimeCached,primes,timing2} = require('./js_utils.js');

// Rotate an array/string
var rotate = function(a,i) {
    var v1 = a.slice(i);
    var v2 = a.slice(0,i);
    return v1.concat(v2);
}

var is_circular_prime = function(n, prime_set) {   
    var s = n.num_to_list2();
    var len = s.length;
    for(var i = 1; i <= len; i++) {
        var v = parseInt(rotate(s,i).join(""));
        if (!prime_set.has(v)) {
            return false;
        }
    }
    return prime_set.has(v);
}

// 220ms
var euler35a = function() {
    var prime_set = new Set(primes(1000000));

    var numCircularPrimes = 0;
    for(var n of prime_set) {
        if (prime_set.has(n) && is_circular_prime(n,prime_set)) {
            numCircularPrimes++;
        }
    }
        
    return numCircularPrimes;
}

timing2(euler35a);



