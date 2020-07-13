/* 

  Run Euler problems in JavaScript.

  This is a port of my traditional Perl wrapper for running Projects Euler problems 1..50.

  - run_euler is an (asynchronous) version but we don't get any statistics
  - run_eulerSync is a synchronous version with the standard statistics:
     * total solve time
     * list of failured programs
     * a list of the programs in reverse order of solve time.

  Here are the solving times (in seconds) as of 2020-07-12:
  euler10.js: 0.351s
  euler30.js: 0.245s
  euler35.js: 0.201s
  euler34.js: 0.19s
  euler36.js: 0.163s
  euler50.js: 0.153s
  euler21.js: 0.149s
  euler14.js: 0.145s
  euler32.js: 0.134s
  euler44.js: 0.065s
  euler12.js: 0.057s
  euler43.js: 0.054s
  euler47.js: 0.042s
  euler37.js: 0.03s
  euler27.js: 0.028s
  euler29.js: 0.025s
  euler23.js: 0.02s
  euler39.js: 0.02s
  euler9.js: 0.018s
  euler49.js: 0.015s
  euler40.js: 0.014s
  euler41.js: 0.011s
  euler7.js: 0.009s
  euler22.js: 0.009s
  euler25.js: 0.009s
  euler48.js: 0.009s
  euler4.js: 0.008s
  euler45.js: 0.007s
  euler31.js: 0.006s
  euler46.js: 0.006s
  euler17.js: 0.004s
  euler18.js: 0.004s
  euler26.js: 0.004s
  euler42.js: 0.003s
  euler38.js: 0.002s
  euler11.js: 0.001s
  euler19.js: 0.001s
  euler20.js: 0.001s
  euler24.js: 0.001s
  euler1.js: 0s
  euler2.js: 0s
  euler3.js: 0s
  euler5.js: 0s
  euler6.js: 0s
  euler8.js: 0s
  euler13.js: 0s
  euler15.js: 0s
  euler16.js: 0s
  euler28.js: 0s
  euler33.js: 0s


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
  Async version.
  Runtime 0.55s

  It's very fast but the changes in the variables (e.g. total_cpu_time, fails) 
  are not propagated.
  So it's not very useful for doing more than basic checking: it exits if an
  answer is wrong.

*/
const run_euler = function() {
    
    let fails = [];
    let total_cpu_time = 0;
    let cpu_times = {};
    
    Object.keys(answers)
        .sort(sortf)
        .forEach(p=>{
            const e = execFile("node", [p], function callback(error, stdout, stderr){
                console.log("stdout: " + stdout.trim());
                const a = answers[p];
                console.log("answer should be: " + a);
                const m2 = stdout.match(re);
                const time = m2[1];                
                const ans = m2[2].replace(/'/g,"");
                total_cpu_time += time;
                cpu_times[p] = time;
                if (ans.toString() == answers[p].toString()) {
                    console.log(p + " CORRECT!");
                } else {
                    fails.push(p);                    
                    console.log(p + " INCORRECT!");
                    process.exit(1);
                }
                console.log(`time: ${time}`);
                console.log(`answer: ${ans}`);                            
                console.log("\n\n");
            });           
    })
    return [fails,total_cpu_time,cpu_times];
}


/*
  Sync version: Here we got the statistics etc.
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


//// Async version takes about 0.55s
// const [fails,total_cpu_time,cpu_times] = run_euler();
// console.log("fails: " + fails);
// console.log("total_cpu_time: " + total_cpu_time);
// console.log("cpu_times: " + Object.keys(cpu_times));

// Sync version takes about 5s to run.
// (The Perl version takes about 4.8s)
const [fails,total_cpu_time,cpu_times] = run_eulerSync();
console.log("\n\nfails: " + fails);
console.log("total_cpu_time: " + total_cpu_time + "s");
console.log("cpu_times:");
Object.keys(cpu_times).sort(function(a,b) {return cpu_times[b]-cpu_times[a]; } )
    .forEach(p=>{
        console.log(`${p}: ${cpu_times[p]}s`)
    });

if (fails.length > 0) {
    console.log("\n\nThe following programs fails: " + fails + "\n\n\n");
}
