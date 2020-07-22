/* 

  Greatest common divisor in JavaScript.

  http://rosettacode.org/wiki/Greatest_common_divisor
  """
  Task

  Find the greatest common divisor   (GCD)   of two integers.


  Greatest common divisor is also known as greatest common factor (gcf)
  and greatest common measure.
  """

  gcd is implemented in js_utils.js.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {gcd,range2} = require('./js_utils.js');

console.log("gcd(10,3) %d:",gcd(10,3));

console.log("gcd(1..20,1..20):");
console.log(
    range2(1,20)
            .map(i=>[i.toString().padStart(2),JSON.stringify(range2(1,20).map(j=>gcd(i,j)))])
);

