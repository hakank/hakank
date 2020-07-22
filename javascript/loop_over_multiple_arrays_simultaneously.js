/* 

  Loop over multiple arrays simultaneously in JavaScript.

  http://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously
  """
  Task

  Loop over multiple arrays (or lists or tuples or whatever they're called 
  in your language) and display the i'th element of each.

  Use your language's "for each" loop if it has one, otherwise iterate 
  through the collection in order with some other loop.

  For this example, loop over the arrays:

    (a,b,c)
    (A,B,C) 
    (1,2,3) 

  to produce the output:

    aA1
    bB2
    cC3


  If possible, also describe what happens when the arrays are of different 
  lengths. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {skip_underfined,skip_undefined2,zip,zip2,transpose2} = require('./js_utils.js');

// a =[ [ 'a', 'b', 'c' ], [ 'A', 'B', 'C' ], [ 1, 2, 3 ] ]
// > a.map((k,i)=>u.range(a.length).map(j=>a[j][i]))
// [ [ 'a', 'A', 1 ], [ 'b', 'B', 2 ], [ 'c', 'C', 3 ] ]

// Different: length: undefined for shorter arrays
// > b =[ [ 'a', 'b', 'c' ], [ 'A', 'B', 'C' ], [ 1, 2] ]
// > b.map((k,i)=>u.range(b.length).map(j=>b[j][i]))
// [ [ 'a', 'A', 1 ], [ 'b', 'B', 2 ], [ 'c', 'C', undefined ] ]

/*
// Moved to js_utils.js
function zipn(a) {
    return a.map((k,i)=>utils.range(a.length).map(j=>a[j][i]))
}
*/

function zip_join(a) {
    return zip(a).map(i=>i.join(""));
}

function zip_join2() {
    return zip(this).map(i=>i.join(""));
}
Array.prototype.zip_join2 = zip_join2;


const a =[ [ 'a', 'b', 'c' ], [ 'A', 'B', 'C' ], [ 1, 2, 3 ] ];
console.log(a);
console.log(zip(a));
console.log(zip_join(a));

console.log("With transpose (which is the same as zip):");
console.log(a.transpose2().map(i=>i.join("")));

console.log()

// Different lengths -> undefined on missing elements
const b =[ [ 'a', 'b', 'c' ], [ 'A', 'B', 'C' ], [ 1, 2] ]
console.log(b);
console.log(b.zip2());
// But we can weed then out by skip_undefined
console.log(b.zip2().skip_undefined2());
// Though when joining the undefined is removed
console.log(b.zip_join2());

console.log();
const c = [1,2,undefined,3,undefined,4,undefined];
console.log(c);
console.log("check skip undefined:", c.skip_undefined2());
console.log("check skip undefined + join:", c.skip_undefined2().join(""));


const d = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]];
console.log(d.zip2());
