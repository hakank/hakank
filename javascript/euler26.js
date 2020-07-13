/* 

  Euler #26 in JavaScript.

  Problem 26:
  """
  A unit fraction contains 1 in the numerator. The decimal representation of the 
  unit fractions with denominators 2 to 10 are given:

      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
  seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
  its decimal fraction part.
  """ 


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {primes,isPrime,max2,range2,timing2} = require('./js_utils.js');

const get_rep_len = function(n) {
    let foundRemainders = new Array(n+1).fill(0);
    let value = 1;
    let position = 1;
    while (foundRemainders[value] === 0 && value !== 0) {
        foundRemainders[value] = position;
        value = (value*10) % n;
        position++;
    }
    return position-foundRemainders[value];
}

// 8ms
const euler26a = function() {
    let maxLen = 0;
    let maxD = 0;
    for (let d = 2; d < 1000; d++) {
        let len = get_rep_len(d);
        if (len > maxLen) {
            maxLen = len;
            maxD = d;
        }        
    }
    return maxD;
}

// Checks only primes
// 5ms
const euler26b = function() {
    let maxLen = 0;
    let maxD = 0;
    for (let d = 2; d < 1000; d++) {
        if (isPrime(d)) {
            let len = get_rep_len(d);
            if (len > maxLen) {
                maxLen = len;
                maxD = d;
            }        
        }
    }
    return maxD;
}

// Only primes
const euler26c = function() {
    return primes(999)
        .map(d=>{return [get_rep_len(d),d]})
        .sort(function(a, b){return b[0]-a[0]})[0][1];
}

// Funktional
// 9ms
const euler26d = function() {
    return range2(2,1000)
        .filter(d=>isPrime(d))
        .map(d=>{return [get_rep_len(d),d]})
        .sort(function(a, b){return b[0]-a[0]})[0][1];
}

// timing2(euler26a); // 8ms
// timing2(euler26b); // 6ms
timing2(euler26c); // 6ms
// timing2(euler26d); // 5ms
