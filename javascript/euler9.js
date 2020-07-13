/* 

  Euler #9 in JavaScript.

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 18ms
const euler9a = function() {
    for(let c = 1; c <= 500; c++) {
        for(let b = 1; b <= c; b++) {
            for(let a = 1; a <= b; a++) {
                if (a+b+c === 1000 && a**2 + b**2 - c**2 == 0) {
                    return a*b*c;
                }
            }
        }
    }
}

timing2(euler9a);

