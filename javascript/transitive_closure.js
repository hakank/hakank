/* 

  Transitive closure (pointer chasing) in JavaScript.

  tc(list,start)

  The list is a graph where the indices points to the next node.

  tc(list,start) returns the transitive closure with start node <start>.


  This was inspired by K:s transitive closure function
  "over until fixed" (\) i.e. list\start
    (2 1 0 4 5 3)\4
    4 5 3

  which is here written as:
    tc([2,1,0,4,5,3],4] 
    => [ 4, 5, 3 ]


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const utils = require('./js_utils.js');

//
// transitive closure on list a with start start
//
function tc(a,start) {
    let t = [start];
    let next = a[t];
    while (!t.includes(next)) {
        t.push(next);
        next = a[next];
    }
    return t;
}

//
// Show the transitive closure for all (unique) elements in a.
//
function tc_all(a) {
    console.log("a: " + a);
    const a2 = [...a].sort(); // sorted list
    let h = [];
    for(let s of a2) {
        if (!h[s]) {
            console.log(s + ": " + tc(a,s));
            h[s]=1;
        }
    }
}

function test() {

    /*
    [ 'a: 1,2,3,4,0', 'start: 0' ]
    [ 0, 1, 2, 3, 4 ]

    [ 'a: 2,1,0,4,5,3', 'start: 4' ]
    [ 4, 5 ]

    [ 'a: 5,2,3,0,1,4', 'start: 5' ]
    [ 5, 4, 1, 2, 3, 0 ]
    */
    const check = [
        // list, start
        [[1,2,3,4,0],0],
        [[2,1,0,4,5,3],4],
        [[5,2,3,0,1,4],5],
    ];

    for (const [a,start] of check) {
        console.log(["a: "+ a, "start: "+start]);
        console.log(tc(a,start));
        console.log()
    }
    
    /*
      a: 5,2,3,0,1,4
      0: 0,5,4,1,2,3
      1: 1,2,3,0,5,4
      2: 2,3,0,5,4,1
      3: 3,0,5,4,1,2
      4: 4,1,2,3,0,5
      5: 5,4,1,2,3,0
    */
    tc_all([5,2,3,0,1,4]);
    
    console.log();
    
    /*
      a: 2,1,0,4,5,4
      0: 0,2
      1: 1
      2: 2,0
      4: 4,5
      4: 4,5
      5: 5,4
    */
    tc_all([2,1,0,4,5,4]);

}


test();
