/* 

  Dot product in JavaScript.

  http://rosettacode.org/wiki/Dot_product
  """
  Task

  Create a function/use an in-built function, to compute the dot product,
  also known as the scalar product of two vectors.

  If possible, make the vectors of arbitrary length.


  As an example, compute the dot product of the vectors:

   [1,  3, -5] and 
   [4, -2, -1] 


  If implementing the dot product of two vectors directly:

  - each vector must be the same length
  - multiply corresponding terms from each vector
  - sum the products (to produce the answer)
  """
  

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {dot_product,zip,zip2,sum,sum2,range2,randomIntArray,timing2} = require('./js_utils.js');


// zip and map and sum2
function dot_product1(a,b) {
    return zip([a,b]).map(([i,j])=>i*j).sum2();
}

// Much faster
/* 
// Moved to js_utils.js
function dot_product2(a,b) {
    let dot = 0;
    for(let i=0; i < a.length; i++) {
        dot += a[i]*b[i];
    }
    return dot;
}
*/
let a = [1,  3, -5];
let b = [4, -2, -1];


console.log(dot_product1(a,b));
console.log(dot_product(a,b));

a = randomIntArray(10_000_000,20);
b = randomIntArray(10_000_000,10);
timing2(() => dot_product1(a,b)); // 694ms
timing2(() => dot_product(a,b)); // 17ms

