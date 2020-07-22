/* 

  Search a list in JavaScript.

  http://rosettacode.org/wiki/Search_a_list
  """
  Task

  Find the index of a string (needle) in an indexable, ordered 
  collection of strings (haystack).

  Raise an exception if the needle is missing.

  If there is more than one occurrence then return the smallest 
  index to the needle.

  Extra credit

  Return the largest index to a needle that has multiple occurrences in the haystack. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const utils = require('./js_utils.js');

let s = ["adam","bertil","cesar","david","bertil"];
console.log(s);

//
// Let's piggyback on JavaScript's indexOf and lastIndexOf
// (which don't throws an exception if not found)
//
function myIndexOf(haystack,needle) {
    const ix = s.indexOf(needle);
    if (ix === -1) {
        throw(Error(`${needle} not found`));
    }
    return ix;
}

function myLastIndexOf(haystack,needle) {
    const ix = s.lastIndexOf(needle);
    if (ix === -1) {
        throw(Error(`${needle} not found`));
    }
    return ix;
}

// Rolling my own
function myIndexOf2(haystack,needle) {
    for(let i = 0; i < haystack.length; i++) {
        if (needle === haystack[i]) {
            return i;
        }
    }
    throw(Error(`${needle} not found`));
}


// Rolling my own
function myLastIndexOf2(haystack,needle) {
    const len = haystack.length;
    for(let i = len-1; i >= 0; i--) {
        if (needle === haystack[i]) {
            return i;
        }
    }
    throw(Error(`${needle} not found`));
}



try {
    console.log("Search for 'bertil (first index)'");
    console.log(myIndexOf(s,"bertil"));

    console.log("Search for 'bertil (last index)'");
    console.log(myLastIndexOf(s,"bertil"));

    console.log("Search for 'adam (last index)'");
    console.log(myLastIndexOf(s,"adam"));

    
    console.log("Search for 'xerxes (first index)'");
    console.log(myLastIndexOf(s,"xerxes"));
} catch(e) {
    console.log(e);
}

console.log("\nnot using built-ins");
try {
    console.log("Search for 'bertil (first index)'");
    console.log(myIndexOf2(s,"bertil"));
    console.log("Search for 'bertil (last index)'");
    console.log(myLastIndexOf2(s,"bertil"));
    console.log("Search for 'adam (last index)'");
    console.log(myLastIndexOf(s,"adam"));

    console.log("Search for 'xerxes (first index)'");
    console.log(myIndexOf2(s,"xerxes"));
} catch(e) {
    console.log(e);
}
