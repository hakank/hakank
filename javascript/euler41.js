/* 

  Euler #41 in JavaScript.

  Problem 41:
  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/
'use strict';
const {all_permutations,isPrime,range2,timing2} = require('./js_utils.js');

// 11ms
var euler41a = function() {
    // Simplification:
    // n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
    // n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
    var n = 7;
    var m = 0;
    while (m == 0 && n >= 4) {
        var p = range2(1,n).reverse();
        for(var pp of all_permutations(p)) {
           var v = parseInt(pp.map(i=>i.toString()).join(""));
           if (isPrime(v)) {
              return v;
           }
        }
        n++;
    }

    return undefined;
}

timing2(euler41a); // 11ms