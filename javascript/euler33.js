/* 

  Euler #33 in JavaScript.

  Problem 33:
  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
  is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than 
  one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find 
  the value of the denominator.
  """ 

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 0ms
const euler33a = function() {
    let s = 1;
    for(let y = 1; y <= 9; y++) {
        for(let z = 1; z <= 9; z++) {
            const x = 9.0*y*z/(10.0*y-z);
            if (1.0*Math.floor(x)===x*1.0 && y/z < 1.0 && x < 10.0) {
                s = (s*y)/z;
            }
        }
    }
    
    return 1/s;
}

timing2(euler33a); // 0ms

