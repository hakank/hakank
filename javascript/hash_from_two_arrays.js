/* 

  Hash from two arrays in JavaScript.

  http://rosettacode.org/wiki/Hash_from_two_arrays
  """
  Task

  Using two Arrays of equal length, create a Hash object where the elements 
  from one array (the keys) are linked to the elements of the other (the values)
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {zip,zipHash} = require('./js_utils.js');


const keys = ["a","b","c","d","e"];
const values = [1,2,3,4,5];

let h = {};
zip([keys,values]).forEach(e=>h[e[0]]=e[1]);
console.log(h);

h = new Map(zip([keys,values]));
console.log(h);

h = {};
keys.map((e,i)=>h[e]=values[i])
console.log(h);

/*
// Moved to js_utils.js
function zipHash(k,v) {
    return Object.fromEntries(zip([k,v]));
}
*/
console.log(zipHash(keys,values));

// This don't work since "k" is not evaluated as the variable
// from the keys array, but as the constant key "k", i.e.
//    [ { k: 1 }, { k: 2 }, { k: 3 }, { k: 4 }, { k: 5 } ]
// h = zip([keys,values]).map(([k,v])=>({k: v}));
// console.log(h);

