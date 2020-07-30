/* 

  Towers of Hanoi in JavaScript.

  http://rosettacode.org/wiki/Towers_of_Hanoi
  """
  Task

  Solve the Towers of Hanoi problem [https://en.wikipedia.org/wiki/Towers_of_Hanoi] 
  with recursion. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {timing2} = require('./js_utils.js');


// We also count the number of recursions
function hanoi(n, print) {
    console.log("n:", n);
    const count = move(n, "left", "center", "right", print, 1);
    console.log("count:", count, "theoretical:", 2**n-1);
    return count;
}

function move(n, from, to, via, print, count) {
    if (n === 0) {
        return 0;
    }

    let count1 = count + move(n-1, from, via, to, print, count);
    if (print) {
        console.log(`Move disk ${n} from pole ${from} to pole ${to}`);
        // process.stdout.write(`${n},`);
    }
    count1 += move(n-1, via, to, from, print, count);
    return count1;
}


hanoi(4,true);

// How many recursions?
console.log("\nTiming n=3..25 showing the number of recursions");
for(let n = 3; n <= 25; n++) {
    timing2(function t() {return hanoi(n,false) } );
}

/*
Move disk 1 from pole left to pole center
Move disk 2 from pole left to pole right
Move disk 1 from pole center to pole right
Move disk 3 from pole left to pole center
Move disk 1 from pole right to pole left
Move disk 2 from pole right to pole center
Move disk 1 from pole left to pole center
Move disk 4 from pole left to pole right
Move disk 1 from pole center to pole right
Move disk 2 from pole center to pole left
Move disk 1 from pole right to pole left
Move disk 3 from pole center to pole right
Move disk 1 from pole left to pole center
Move disk 2 from pole left to pole right
Move disk 1 from pole center to pole right
Move disk 5 from pole left to pole center
Move disk 1 from pole right to pole left
Move disk 2 from pole right to pole center
Move disk 1 from pole left to pole center
Move disk 3 from pole right to pole left
Move disk 1 from pole center to pole right
Move disk 2 from pole center to pole left
Move disk 1 from pole right to pole left
Move disk 4 from pole right to pole center
Move disk 1 from pole left to pole center
Move disk 2 from pole left to pole right
Move disk 1 from pole center to pole right
Move disk 3 from pole left to pole center
Move disk 1 from pole right to pole left
Move disk 2 from pole right to pole center
Move disk 1 from pole left to pole center
count: 31 theoretical: 31
*/
// hanoi(5,true);
