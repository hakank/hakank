/* 

  SEND+MORE=MONEY in JavaScript.

  Solution should be: 
  [[9, 5, 6, 7, 1, 0, 8, 2]]


  Here are four different approaches:
  - using next_permutation (fastest). 127ms
  - using permutation (slowest): 5504ms
  - using sets: 748ms
  - using list and all_different: 2374ms

  The two first approaches are more elegant IMHO...

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {all_different,range,all_permutations,next_permutation,timing2} = require('./js_utils.js');


// next_permutation: 127ms
function send_more_money1() {
    let p = range(10);
    let sols = [];
    while (p != null) {
        let [s,e,n,d,m,o,r,y,a,b] = p; // a and b are dummy variables
        if (
            s > 0 &&
                m > 0 &&
                (s * 1000 + e * 100 + n * 10 + d) +
                (m * 1000 + o * 100 + r * 10 + e) ===
                (m * 10000 + o * 1000 + n * 100 + e * 10 + y ) &&
                a < b 
        ) {
            sols.push([s,e,n,d,m,o,r,y]);
        }
        
        p = next_permutation(p);
    }

    return sols;
}

// all_permutation: 5466ms
function send_more_money2() {
    let sols = [];
    const allp = all_permutations(range(10));
    for (let p of allp) {
        let [s,e,n,d,m,o,r,y,a,b] = p; // a and b are dummy variables
        if (
            s > 0 &&
                m > 0 &&
                (s * 1000 + e * 100 + n * 10 + d) +
                (m * 1000 + o * 100 + r * 10 + e) ===
                (m * 10000 + o * 1000 + n * 100 + e * 10 + y ) &&
                a < b 
        ) {
            sols.push([s,e,n,d,m,o,r,y]);
        }
    }

    return sols;
}

//
// Return a set with all the elements in elements removed
//
function exclude(s,elements) {
    let s2 = new Set(s);
    for (const e of elements) {
        s2.delete(e);
    }
    
    return s2;
}

//
// Set approach: 748ms
//
function send_more_money3() {
    let sols = [];
    const ss = new Set(range(10));
    for (let s of ss) {
        for (let e of exclude(ss,[s])) {
            for (let n of exclude(ss,[s,e])) {
                for (let d of exclude(ss,[s,e,n])) {
                    for (let m of exclude(ss,[s,e,n,d])) {
                        for (let o of exclude(ss,[s,e,n,d,m])) { 
                            for (let r of exclude(ss,[s,e,n,d,m,o])) {
                                for (let y of exclude(ss,[s,e,n,d,m,o,r])) {
                                    if (          s > 0 && m > 0 &&
                                                  (s * 1000 + e * 100 + n * 10 + d) +
                                                  (m * 1000 + o * 100 + r * 10 + e) == 
                                                  (m * 10000 + o * 1000 + n * 100 + e * 10 + y ))
                                    {
                                        sols.push([s,e,n,d,m,o,r,y]);
                                    }
                                }                           
                            }                           
                        }
                    }
                }
            }
        }
    }
    
    return sols;

}


//
// Using all_different(); 2374ms
//
function send_more_money4() {
    let sols = [];
    const ss = range(10);
    for (let s of ss) {
        for (let e of ss) {
            for (let n of ss) {
                for (let d of ss) {
                    for (let m of ss) {
                        for (let o of ss) { 
                            for (let r of ss) {
                                for (let y of ss) {
                                    if (all_different([s,e,n,d,m,o,r,y]) &&
                                        s > 0 && m > 0 &&
                                                  (s * 1000 + e * 100 + n * 10 + d) +
                                                  (m * 1000 + o * 100 + r * 10 + e) == 
                                                  (m * 10000 + o * 1000 + n * 100 + e * 10 + y ))
                                    {
                                        sols.push([s,e,n,d,m,o,r,y]);
                                    }
                                }                           
                            }                           
                        }
                    }
                }
            }
        }
    }
    
    return sols;

}


timing2(send_more_money1); // 127ms
// timing2(send_more_money2); // 5466ms
timing2(send_more_money3); // 748ms
// timing2(send_more_money4); // 2374ms
