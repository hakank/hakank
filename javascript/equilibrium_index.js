/* 

  Equilibrium index in JavaScript.

  http://rosettacode.org/wiki/Equilibrium_index
  """
  An equilibrium index of a sequence is an index into the sequence 
  such that the sum of elements at lower indices is equal to the sum of elements at higher indices.

  For example, in a sequence A:

  A0 = −7
  A1 = 1
  A2 = 5
  A3 = 2
  A4 = −4
  A5 = 3
  A6 = 0

  3 is an equilibrium index, because:

   A0 + A1 + A2 = A4 + A5 + A6 

  6 is also an equilibrium index, because:

  A0 + A1 + A2 + A3 + A4 + A5 = 0

  (sum of zero elements is zero)

  7 is not an equilibrium index, because it is not a valid index of 
  sequence A

  Task;

  Write a function that, given a sequence, returns its equilibrium 
  indices (if any).

  Assume that the sequence may be very long. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {sum2,randomIntArray,randomInt,range,timing2} = require('./js_utils.js');

// This is slow for large array (1_000_000)
function equilibrium_index(a) {
    let res = [];
    for(let i = 0; i < a.length; i++) {
        if (a.slice(0,i).sum2() === a.slice(i+1).sum2()) {
            res.push(i);
        }
    }
    return res;
}

// Keep cumulative sums of the left and right parts.
// Much faster.
function equilibrium_index2(a) {
    let res = [];
    let left = a[0];
    let right = a.slice(1).sum2();
    for(let i = 1; i < a.length; i++) {
        right -= a[i];
        if (left === right) {
            res.push(i);
        }
        left  += a[i];
    }
    return res;
}


let a = [-7, 1, 5, 2, -4, 3, 0];
console.log(a);
console.log(equilibrium_index(a));
console.log(equilibrium_index2(a));

// Generates in the range -max .. max
function randomIntArrayNeg(n,max) {
    return range(n).map(i=>randomInt(2*max)-max);
}

a = randomIntArrayNeg(20_000,100);
timing2(function test_equilibrium_index() { equilibrium_index(a) }); // 1366ms
timing2(function test_equilibrium_index2() { equilibrium_index2(a) }); // 1ms

// Much larger instance
a = randomIntArrayNeg(10_000_000,100);
timing2(function test_equilibrium_index2_2() { equilibrium_index2(a)}); // 74ms

