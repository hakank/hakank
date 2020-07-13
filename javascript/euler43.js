/* 

  Euler #43 in JavaScript.

  """  
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
  
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
  
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
  
  Find the sum of all 0 to 9 pandigital numbers with this property.
  """

  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {all_permutations,next_permutation,range,range2,timing2} = require('./js_utils.js');

//
// Using all_permutations: 5214ms (5.2s)
//
const euler43a = function() {
    const primes = [2,3,5,7,11,13,17];
    const perms = all_permutations(range2(0,9));
    let sum = 0;
    for(let p of perms) {
        let i = 0;
        let found = true;
        while(i < 7 && found === true) {
            if ( (100*p[i+1] + 10*p[i+2] + p[i+3]) % primes[i] !== 0) {
                found = false;
                break;
            }
            i++;
        }

        if (found) {
            console.log("found " + p)
            sum += parseInt(p.join(""));
        }
    }

    return sum;
}

//
// Using next_permutations: 54ms (rather faster :-))
//
const euler43b = function() {
    const primes = [2,3,5,7,11,13,17];
    let p = [1,0,2,3,4,5,6,7,8,9];
    let sum = 0;
      while(p !== null) {
        let i = 0;
        let found = true;
        while(i < 7 && found === true) {
            if ( (100*p[i+1] + 10*p[i+2] + p[i+3]) % primes[i] !== 0) {
                found = false;
                break;
            }
            i++;
        }

        if (found) {
            sum += parseInt(p.join(""));
        }
        p = next_permutation(p);
    }

    return sum;
}


// timing2(euler43a); // 5.2s
timing2(euler43b); // 54ms

