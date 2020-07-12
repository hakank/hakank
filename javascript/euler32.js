/* 

  Euler #32 in JavaScript.

  Problem 32
  """
  We shall say that an n-digit number is pandigital if it makes use of 
  all the digits 1 to n exactly once; for example, the 5-digit number, 
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
  containing multiplicand, multiplier, and product is 1 through 9 
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product 
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {sum2,timing2} = require('./js_utils.js');

// 151ms
var euler32a = function() {
    var prod_hash = {};
    for(var a =2; a <= 98; a++) {
        var as = a.toString();
        for(var b = a+1; b <= 9876; b++) {
            var p = a*b;
            var l = as + b.toString() + p.toString();
            if (l.length == 9 && !l.match(/0/)) {
                if (new Set(l).size == 9) {
                    prod_hash[p] = 1;
                }
            }
        }
    }

    return Object.keys(prod_hash).map(i=>parseInt(i)).sum2();
}

timing2(euler32a); // 151ms

