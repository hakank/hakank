/* 

  Pascal's triangle in JavaScript.

  http://rosettacode.org/wiki/Pascal%27s_triangle
  """
  Pascal's triangle is an arithmetic and geometric figure often associated 
  with the name of Blaise Pascal, but also studied centuries earlier in India, 
  Persia, China and elsewhere.

  Its first few rows look like this:

    1
   1 1
  1 2 1
 1 3 3 1

  where each element of each row is either 1 or the sum of the two elements 
  right above it.

  For example, the next row of the triangle would be:

   1   (since the first element of each row doesn't have two elements above it)
   4   (1 + 3)
   6   (3 + 3)
   4   (3 + 1)
   1   (since the last element of each row doesn't have two elements above it) 

  So the triangle now looks like this:

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

  Each row n (starting with row 0 at the top) shows the coefficients of the 
  binomial expansion of   (x + y)n.


  Task

  Write a function that prints out the first n rows of the triangle   
  (with f(1) yielding the row consisting of only the element 1).

  This can be done either by summing elements from the previous rows or 
  using a binary coefficient or combination function.

  Behavior for n <= 0 does not need to be uniform, but should be noted. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {binomial,binomialN,range2,range2N} = require('./js_utils.js');


// Integer version: Handles up to n=73.
function pascal(n) {
    return range2(0,n)
        .map(m=>
             range2(0,m).map(k=>binomial(m,k)));
}

// BigInt version
function pascalN(n) {
    return range2(0,n)
        .map(m=>range2N(0n,BigInt(m)).map(k=>binomialN(BigInt(m),BigInt(k))));
}

console.log(pascal(10).map(p=>p.join(" ")));
// console.log(pascal(73).map(p=>p.join(" "))); // max for integers

pascalN(200)
    .forEach(p=>console.log(p.join(" "))); // BigInt
