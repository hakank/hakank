/* 

  Find the missing permutation in JavaScript.

  http://rosettacode.org/wiki/Find_the_missing_permutation
  """
  ABCD
  CABD
  ACDB
  DACB
  BCDA
  ACBD
  ADCB
  CDAB
  DABC
  BCAD
  CADB
  CDBA
  CBAD
  ABDC
  ADBC
  BDCA
  DCBA
  BACD
  BADC
  BDAC
  CBDA
  DBCA
  DCAB
  
  Listed above are all-but-one of the permutations of the symbols A, B, C, and D, except
  for one permutation that's not listed.


 Task

 Find that missing permutation. 


 Methods

 - Obvious method: 

   enumerate all permutations of A, B, C, and D,  
   and then look for the missing permutation. 

 - alternate method:

   Hint:  if all permutations were shown above,  how many 
   times would  A  appear in each position?     
   What is the parity of this number?

 - another alternate method:

   Hint:  if you add up the letter values of each column, 
   does a missing letter A, B, C, and  D from each
   column cause the total value for each column to be unique?
 """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {all_permutations,difference} = require('./js_utils.js');

const perms = [
    "ABCD",
    "CABD",
    "ACDB",
    "DACB",
    "BCDA",
    "ACBD",
    "ADCB",
    "CDAB",
    "DABC",
    "BCAD",
    "CADB",
    "CDBA",
    "CBAD",
    "ABDC",
    "ADBC",
    "BDCA",
    "DCBA",
    "BACD",
    "BADC",
    "BDAC",
    "CBDA",
    "DBCA",
    "DCAB"];


const allp = all_permutations("ABCD").map(s=>s.join(""));

// Obvious approaches
console.log(allp.filter(p=>perms.indexOf(p)<0));

// As a one-liner
console.log(all_permutations("ABCD").map(s=>s.join("")).filter(p=>perms.indexOf(p)<0));

// Slighly less obvious (since it's not using permutations)
console.log(difference(allp,perms));


// alternative method: count the number of occurrences of the characters in each position
let h = {};
for(let p of perms) {
    for(let i = 0; i < 4; i++) {
        if (h[[i,p[i]]] === undefined) {
            h[[i,p[i]]] = 0;
        }
        h[[i,p[i]]]++;
    }
}

// console.log(h);
let missing = ["","","",""];
for(let [ic,v] of Object.entries(h)) {
    if (v === 5) {
        ic = ic.split(",");
        missing[ic[0]] = ic[1]; 
    }
}
console.log(missing.join(""));
