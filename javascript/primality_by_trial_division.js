/* 

  Primality by trial division in JavaScript.

  http://rosettacode.org/wiki/Primality_by_trial_division
  """
  Task

  Write a boolean function that tells whether a given integer is prime.

  Remember that 1 and all non-positive numbers are not prime.

  Use trial division.

  Even numbers over two may be eliminated right away.

  A loop from 3 to sqrt(n) will suffice, but other loops are allowed.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2,timing2} = require('./js_utils.js');


// Time for 10**6: 190ms
// Time for 10**7: 3411ms
function isPrimeByTrialDivision(n) {
    if (n <= 1) {
        return false;
    }
    
    if (n === 2 || n === 3) {
        return true;
    }

    if (n % 2 === 0) {
        return false;
    }
    const m = Math.ceil(Math.sqrt(n));
    for(let i = 3; i <= m; i+=2) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

// Inspired by the Raku variant
// Much slower. Time for 10**6: 20.665s
function isPrimeFunctional(n) {
    return n > 1 && range2(2,Math.floor(Math.sqrt(n))).every(i=>n%i);
}

console.log("isPrimeByTrialDivision for <=100");
console.log(range2(1,100)
            .filter(n=>isPrimeByTrialDivision(n)));

console.log("\nisPrimeFunctional for <=100");
console.log(range2(1,100)
            .filter(n=>isPrimeFunctional(n)));

console.log("\nTiming of isPrimeByTrialDivision: number of primes below 10**i:");
for(let i = 1; i <= 7; i++) {
    const m=10**i;
    console.log(`\nTest 10**${i}=${m}`);
    timing2(function t() {
        return range2(1,m)
                    .filter(n=>isPrimeByTrialDivision(n)).length;
        
    })
}

// 20.665s
console.log("\niTiming sPrimeFunctional(10**6)");
timing2(function t() {
    const m = 10**6;
    return range2(1,m)
        .filter(n=>isPrimeFunctional(n)).length;
       })

