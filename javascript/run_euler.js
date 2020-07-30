/* 

  Run Euler problems in JavaScript.

  Note: This program don't do any checks etc, just running (loading) 
        the euler*.js programs.

  Update: The program do checks but not any statistics.
          Also, it runs async so the results are printed in
          (about) the order of how long it took.

          It is fast: 0.54s but running async might be cheating.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2} = require('./js_utils.js');
const {execFile} = require('child_process');

/*
// Original program:
range2(1,50)
    .forEach(n=> {
        const euler = require(`./euler${n}.js`);
    })
*/

//
// The programs and the correct answers.
//
const answers = {
    "euler1.js":233168,
    "euler2.js":4613732,
    "euler3.js":6857,
    "euler4.js":906609,
    "euler5.js":232792560,
    "euler6.js":25164150,
    "euler7.js":104743,
    "euler8.js":40824,
    "euler9.js":31875000,
    "euler10.js":142913828922,
    "euler11.js":70600674,
    "euler12.js":76576500,
    "euler13.js":5537376230,
    "euler14.js":837799,
    "euler15.js":137846528820,
    "euler16.js":1366,
    "euler17.js":21124,
    "euler18.js":1074,
    "euler19.js":171,
    "euler20.js":648,
    "euler21.js":31626,
    "euler22.js":871198282,
    "euler23.js":4179871,
    "euler24.js":2783915460,
    "euler25.js":4782,
    "euler26.js":983,
    "euler27.js":-59231,
    "euler28.js":669171001,
    "euler29.js":9183,
    "euler30.js":443839,
    "euler31.js":73682,
    "euler32.js":45228,
    "euler33.js":100,
    "euler34.js":40730,
    "euler35.js":55,
    "euler36.js":872187,
    "euler37.js":748317,
    "euler38.js":932718654,
    "euler39.js":840,
    "euler40.js":210,
    "euler41.js":7652413,
    "euler42.js":162,
    "euler43.js":16695334890,
    "euler44.js":5482660,
    "euler45.js":1533776805,
    "euler46.js":5777,
    "euler47.js":134043,
    "euler48.js":9110846700,
    "euler49.js":296962999629,
    "euler50.js":997651
    // "euler50.js":997651000000 // for testing bad solutions.
}

const re = /\[\s(\d+?),\s(.+?)\s\]/m;

let times = {};

function run_euler() {
    range2(1,50)
        .forEach(n=>{
            const p = `euler${n}.js`;
            execFile("node", [p], function callback(error, stdout, stderr){
                console.log(`stdout for euler${n}.js: ` + stdout.trim());
                const a = answers[p];
                console.log("answer should be: " + a);
                const m2 = stdout.match(re);
                const time = m2[1];
                times[p] = time;
                const ans = m2[2].replace(/'/g,"");
                if (ans.toString() == answers[p].toString()) {
                    console.log(p + " CORRECT!");
                } else {
                    // fails.push(p);                    
                    console.log(p + " INCORRECT!");
                    process.exit(1);
                }
                console.log(`time: ${time}`);
                console.log(`answer: ${ans}`);
                console.log("\n\n");
            })
        })
}

run_euler();
console.log("TIMES:")
console.log(times);
