/* 

  Run Euler problems in JavaScript.

  This is a port of my traditional Perl wrapper for running Projects Euler problems 1..50.

  - run_eulerSync is a synchronous version with the standard statistics:
     * total solve time
     * list of failured programs (if any)
     * a list of the programs in reverse order of solve time.

  Here are the solving times (in seconds) as of 2020-07-14:
  total_cpu_time: 1.9999999999999993s
  cpu_times:
  euler10.js: 0.253s
  euler30.js: 0.232s
  euler35.js: 0.194s
  euler34.js: 0.17s
  euler14.js: 0.155s
  euler50.js: 0.146s
  euler32.js: 0.136s
  euler21.js: 0.135s
  euler36.js: 0.075s
  euler12.js: 0.069s
  euler44.js: 0.065s
  euler43.js: 0.053s
  euler47.js: 0.042s
  euler29.js: 0.035s
  euler27.js: 0.033s
  euler37.js: 0.03s
  euler23.js: 0.021s
  euler39.js: 0.019s
  euler9.js: 0.016s
  euler40.js: 0.015s
  euler49.js: 0.015s
  euler41.js: 0.01s
  euler4.js: 0.009s
  euler7.js: 0.009s
  euler22.js: 0.009s
  euler25.js: 0.009s
  euler48.js: 0.009s
  euler45.js: 0.008s
  euler31.js: 0.005s
  euler18.js: 0.004s
  euler26.js: 0.004s
  euler46.js: 0.004s
  euler17.js: 0.003s
  euler42.js: 0.003s
  euler38.js: 0.002s
  euler5.js: 0.001s
  euler11.js: 0.001s
  euler33.js: 0.001s
  euler1.js: 0s
  euler2.js: 0s
  euler3.js: 0s
  euler6.js: 0s
  euler8.js: 0s
  euler13.js: 0s
  euler15.js: 0s
  euler16.js: 0s
  euler19.js: 0s
  euler20.js: 0s
  euler24.js: 0s
  euler28.js: 0s


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2} = require('./js_utils.js');
const {exec,execSync,execFile,execFileSync} = require('child_process');

const program = "node";

/*
   For some reason, JavaScript don't allow `...` as value in a hash 
   so these variables are not used
*/
// const prefix = "euler";
// const ext = "js";


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
// const re = /\[\s((?<time>\d+?)),\s((?<answer>.+?))\s\]/m; // don't work (as I expect)
console.log("re: " + re);

const sortf = function(a,b) {
    const anum = a.match(/euler(\d+).js/);
    const bnum = b.match(/euler(\d+).js/);
    return anum-bnum;
}


/*
  Sync version: Show the statistics, summaries etc.
  Runtime is about 5s but we - still - got the total timing of 2.25s
*/
const run_eulerSync = function() {
    
    let fails = [];
    let total_cpu_time = 0;
    let cpu_times = {};
    
    Object.keys(answers)
        .sort(sortf)
        .forEach(p=>{
            const stdout = execSync(`node ${p}`).toString();
            console.log("stdout: " + stdout.trim());
            const a = answers[p];
            console.log("answer should be: " + a);
            const m2 = stdout.match(re);
            const time = parseInt(m2[1])/1000.0; // convert to seconds               
            const ans = m2[2].replace(/'/g,"");
            total_cpu_time += time;
            cpu_times[p] = time;
            if (ans.toString() == answers[p].toString()) {
                console.log(p + " CORRECT!");
            } else {
                fails.push(p);                    
                console.log(p + " INCORRECT!");
                // process.exit(1);
            }
            console.log(`time: ${time}`);
            console.log(`answer: ${ans}`);                            
            console.log("\n\n");
        });
    
    return [fails,total_cpu_time,cpu_times];
}

//
// Sync version takes about 5s to run.
// (The Perl version takes about 4.8s)
// The total solve time is around 2.0s
//
const [fails,total_cpu_time,cpu_times] = run_eulerSync();
console.log("\n\nfails: " + fails);
console.log("total_cpu_time: " + total_cpu_time + "s");
console.log("cpu_times:");
Object.keys(cpu_times).sort(function(a,b) {return cpu_times[b]-cpu_times[a]; } )
    .forEach(p=>{
        console.log(`${p}: ${cpu_times[p]}s`)
    });

console.log("\ntotal_cpu_time: " + total_cpu_time + "s\n");

if (fails.length > 0) {
    console.log("\n\nThe following programs fails: " + fails + "\n\n\n");
}

