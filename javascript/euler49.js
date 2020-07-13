/* 

  Euler #49 in JavaScript.

  Problem 49:
  """  
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
  increases by 3330, is unusual in two ways: (i) each of the three terms are 
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms 
  in this sequence?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {isPrime,all_permutations,timing2} = require('./js_utils.js');

const check_perms = function(n, diff) {
    const allperms = all_permutations(n.toString().split(""));
    if (allperms.length > 0) {
        const p1 = get_element(n, allperms, diff);
        if (p1 !== undefined) {
            const p2 = get_element(p1, allperms, diff);
            if (p2 !== undefined) {
                return [n, p1, p2];
            }
        }
    }

    return undefined;
}

const get_element = function(n, ll, diff) {
    let res = 0;
    for(let p of ll) {
        const pp = parseInt(p.map(i=>i.toString()).join(""));
        if (isPrime(pp) && pp > n && pp-n === diff) {
            return pp;
        }
    }
    return undefined;
}

// 19ms
const euler49a = function() {
    const diff = 3330;
    let res = 0;
    for(let n = 1001; n <= 9999; n+=2) {
        if (n !== 1487 && isPrime(n) ) {
            const c = check_perms(n, diff);
            if (c !== undefined) {
                res = c;
                break;
            }
        }
    }

    return parseInt(res.join(""));

}


timing2(euler49a); // 19ms
