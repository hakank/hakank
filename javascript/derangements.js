/* 

  Permutations/Derangements in JavaScript.

  http://rosettacode.org/wiki/Permutations/Derangements
  """
  Permutations/Derangements
  A derangement is a permutation of the order of distinct items in which 
  no item appears in its original place.

  For example, the only two derangements of the three items 
    (0, 1, 2) are (1, 2, 0), and (2, 0, 1).
  
  The number of derangements of n distinct items is known as the subfactorial of n, 
  sometimes written as !n. There are various ways to calculate !n.

  Task

  The task is to:

   - Create a named function/method/subroutine/... to generate derangements of the integers 
     0..n-1, (or 1..n if you prefer).
   - Generate and show all the derangements of 4 integers using the above routine.
   - Create a function that calculates the subfactorial of n, !n.
   - Print and show a table of the counted number of derangements of n vs. the calculated 
     !n for n from 0..9 inclusive. 

  As an optional stretch goal:

  -  Calculate !20. 
  """

  See js_utils.js for implementation details of subfactorial(N).


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {next_permutation,subfactorial,subfactorialN,subfactorialNCached,factorial,memoizer,range,range2} = require('./js_utils.js');

function is_derangement(p) {
    for(let i = 0; i < p.length; i++) {
        if (p[i] === i) {
            return false;
        }
    }

    return true;
}

function derangements(p) {
    let d = [];
    while (p !== null) {
        if (is_derangement(p)) {
            d.push([...p]);
        }
        p = next_permutation(p);
    }
    return d;
}


function derangements_len(p) {
    return derangements(p).length
}

function subfactorial_approx(n) {
    return Math.floor(1*Math.floor(factorial(n)/Math.E + 1/2))
}

console.log(derangements(range(4)));
console.log("length:", derangements_len(range(4)));

console.log("length of derangements(0..9)", derangements_len(range(10)));


// OEIS: https://oeis.org/A000166
// 1, 0, 1, 2, 9, 44, 265, 1854, 14833, 133496, 1334961, 14684570, 176214841, 2290792932, ... 
for (let n = 0; n <= 30; n++) {
    const s = subfactorialNCached(BigInt(n));
    const sa = subfactorial_approx(n);
    const diff = sa < Number.MAX_SAFE_INTEGER ? s - BigInt(sa) : " sa too large to be safe";
    console.log(n, s ," ", sa, " ", diff);
}

console.log("!1000:", subfactorialNCached(1000n));
