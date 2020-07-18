/* 

  N-queens problem in JavaScript.


  Timing for n=2..12, in seconds:

  queens (using next_permutation):
  [n, time, #solutions]
  2 0.001 0
  3 0 0
  4 0 2
  5 0 10
  6 0.005 4
  7 0 40
  8 0.004 92
  9 0.04 352
  10 0.452 724
  11 5.554 2680
  12 73.517 14200

  queens2 (using all_permutations) is much slower:
  [n, time, #solutions]
  2 0 0
  3 0 0
  4 0 2
  5 0 10
  6 0.006s 4
  7 0.008s 40
  8 0.062s 92
  9 0.561s 352
  10 5.758s  724
  And for n=11 the memory is exhausted...


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {range,range2,all_permutations,next_permutation,timing2} = require('./js_utils.js');

// Brute force version using next_permutation
function queens(n,print) {
    const t_start = +new Date(); // new Date().getTime();    
    let q = range(n);
    let c = 0;
    while (q !== null) {
        let check = true;
        loop: 
        for(let i = 0; i < n; i++) {
            for(let j = 0; j < i; j++) {
                if (q[i] === q[j] ||
                    q[i] + i === q[j] + j ||
                    q[i] - i === q[j] - j
                   ) {
                    check = false;
                    continue loop;
                }
            }
        }
        if (check === true) {
            c++;
            if (print) {
                console.log(q.join(","));
            }
        }
        q = next_permutation(q);
    }
    const t_end = +new Date();
    const t = (t_end-t_start)/1000.0;
    if (print) {
        console.log([t,c]);
    }
    
    return [t, c];
}


// Brute force version using all_permutations
// Much slower.
function queens2(n,print) {
    const t_start = +new Date(); // new Date().getTime();    
    let allp = all_permutations(range(n));
    let c = 0;
    for(const q of allp) {
        let check = true;
        loop: 
        for(let i = 0; i < n; i++) {
            for(let j = 0; j < i; j++) {
                if (q[i] === q[j] ||
                    q[i] + i === q[j] + j ||
                    q[i] - i === q[j] - j
                   ) {
                    check = false;
                    continue loop;
                }
            }
        }
        if (check === true) {
            c++;
            if (print) {
                console.log(q.join(","));
            }
        }
    }
    const t_end = +new Date();
    const t = (t_end-t_start)/1000.0;
    if (print) {
        console.log([t,c]);
    }
    
    return [t, c];
}

function run_queens(qf) {
    let times = [];
    for(let n of range2(2,12)) {
        console.log("\nn: " + n);
        const [t,num_sols] = qf(n,false);
        times.push([n,t,num_sols]);
        console.log("time: " + t + "s  #solutions:", num_sols);
    }
    
    console.log("[n, time, #solutions]");
    for (let t of times) {
        console.log(t.join(" "));
    }
    
}


// functional variant
function run_queens2(qf) {
    console.log("[n, time, #solutions]");    
    const res = range2(2,11)
          .map(n=>console.log([n,...qf(n,false)]));
}


// run_queens(queens);
// run_queens(queens2); // slow and crash (memory)
run_queens2(queens); // more functional than run_queens/1

// queens(8,true);
