/* 

  Münchausen numbers in JavaScript.

  http://rosettacode.org/wiki/Munchausen_numbers
  """
  A Munchausen number is a natural number n the sum of whose digits 
  (in base 10), each raised to the power of itself, equals n.

  (Munchausen is also spelled: Münchhausen.)

  For instance:   3435 = 33 + 44 + 33 + 55

  Task

  Find all Munchausen numbers between 1 and 5000
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2,sum2,timing2} = require('./js_utils.js');

function isMunchhausenNumber(n) {
    // return n === parseInt(n.toString().split("").map(i=>i**i).sum2());
    // shorter with '==' and without parseInt
    return n == n.toString().split("").map(i=>i**i).sum2();
}

// Much faster.
// From the Kotlin version.
function testlarger() {
    // Caching 
    let h = new Array(10).fill(0);
    // Ah, 0**0 is assumed to be 0 here!
    for(let i = 1; i <= 9; i++) {
        h[i] = i**i;
    }
    function isMunchhausenNumber2(n) {
        if (n < 0) {
            return false;
        }
        let sum = 0;
        let nn = n;
        while (nn > 0) {
            sum += h[nn % 10];
            if (sum > n) {
                return false;
            }
            nn = Math.floor(nn / 10);
        }
        return sum === n;
    }

    let res = [];
    for(let n=1; n < 500_000_000; n++) {
        if (isMunchhausenNumber2(n)) {
            res.push(n);
        }
    }
    return res;
}

// [ 1, 3435 ]
console.log(range2(1,5000).filter(n=>isMunchhausenNumber(n)));

// [ 1, 3435, 438579088 ]
// 17.764s
timing2(testlarger);
