/* 

  Remove duplicate elements in JavaScript.

  http://rosettacode.org/wiki/Remove_duplicate_elements
  """
  Given an Array, derive a sequence of elements in which all 
  duplicates are removed.

  There are basically three approaches seen here:
  - Put the elements into a hash table which does not allow duplicates. 
    The complexity is O(n) on average, and O(n2) worst case. This approach 
    requires a hash function for your type (which is compatible with equality), 
    either built-in to your language, or provided by the user.

  - Sort the elements and remove consecutive duplicate elements. The complexity 
    of the best sorting algorithms is O(n log n). This approach requires that 
    your type be "comparable", i.e., have an ordering. Putting the elements into 
    a self-balancing binary search tree is a special case of sorting.

  - Go through the list, and for each element, check the rest of the list to see if 
    it appears again, and discard it if it does. The complexity is O(n2). 
    The up-shot is that this always works on any type (provided that you can 
    test for equality).
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {remove_duplicates,remove_duplicates2,timing2,randomIntArray} = require('./js_utils.js');


/*
// Moved to js_utils.js
// Hash version: This is faster
function remove_duplicates(a) {
    let h = {};
    let res = [];
    for(let e of a) {
        if (!h[e]) {
            res.push(e);
            h[e] = 1;
        }
    }
    return res;
}
*/

// Check if e is a member of a
function member(a,e) {
    return a.indexOf(e) === -1;
}

// Functional recursive variant: quite slower
function remove_duplicates_rec([h,...t]) {
    if (t.length === 0) {
        return [];
    } else {
        return [h].concat(remove_duplicates_rec(t.filter(e=>e!==h)));
    }
}

// Another approach: check if the element has occurred
// Too slow
function remove_duplicates3(a) {
    const len= a.length;
    let res = [];
    for(let i = 0; i < len; i++) {
        if (a.slice(0,i).indexOf(a[i]) === -1) {
            res.push(a[i]);
        }
    }
    return res;    
}


let s = [1,1,2,3,2,1,3,3,4,4,1,5,5,2,2,2];
console.log(s);
console.log(remove_duplicates(s));

console.log("_rec\n");
console.log(remove_duplicates_rec(s));

console.log("v3:");
console.log(remove_duplicates3(s));

// Timing for a larger array
s = randomIntArray(1_000_000,22);
console.log("Timing remove_duplicates");
timing2(() => s.remove_duplicates2()); // 16ms
console.log("Timing remove_duplicates_rec");
timing2(() => remove_duplicates_rec(s)); // 453ms
// console.log("Timing v3");
// timing2(() => remove_duplicates3(s)); // Well, don't! It's too slow.
