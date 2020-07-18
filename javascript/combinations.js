/* 

  Combinations in JavaScript.

   http://rosettacode.org/wiki/Combinations
   """
   Given non-negative integers m and n, generate all size m combinations of the 
   integers from 0 to n-1 in sorted order (each combination is sorted and the 
   entire table is sorted).
  
   For example, 3 comb 5 is
   
   0 1 2
   0 1 3
   0 1 4
   0 2 3
   0 2 4
   0 3 4
   1 2 3
   1 2 4
   1 3 4
   2 3 4
   
   If it is more "natural" in your language to start counting from 1 instead of 0 
   the combinations can be of the integers from 1 to n. 
   """

  See the implementation of combinations/2 in js_utils.js
  

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {odometer,combinations,range,range2} = require('./js_utils.js');


const res = combinations(3,range(5))
for(let r of res) {
    console.log(r);
}
console.log(res.length);

console.log(combinations(3,"håkan"))
console.log(combinations(3,[1,2,3,4,5]))

console.log(range2(3,10).map(n=>combinations(3,n).length));
console.log(range2(0,10).map(n=>combinations(n,10).length));
console.log(range2(0,20).map(n=>combinations(n,20).length));
