/*
  Read test in JavaScript.

  This is one of my standard test when learning a new programming
  language: read a word file and filters the words that match 
  the regular expression
      a.*b.*c..., b.*c.*d.., c.*d.*e..., etc


  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/


*/
'use strict';
const {range,timing} = require("./js_utils.js");

const fs = require("fs");
let lang = "swe";
let wordlist = "words_lower.txt";
let alpha = "abcdefghijklmnopqrstuvwxyz".split(""); // English (etc) words
const n = 5;

if (lang === "swe") {
    wordlist = "/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt";
    alpha = "abcdefghijklmnopqrstuvwxyzåäö".split(""); // Swedish words    
}

const words = fs.readFileSync(wordlist,"utf8").split("\n");
console.log("number of words: " + words.length);

//
// for loop with some functional ingredients
//
const forloop = function() {

    for (let i = 0; i < alpha.length - n; i++) {
        const r = new RegExp(alpha.slice(i,i+n).join(".*"));
        console.log("\t" + r);
        const m = words.filter(word =>r.test(word));
        // if (m.length > 0) {
        console.log(m.toString());
        console.log(" len: " + m.length + "\n");
        // }
    }
}

// A more functional approach, a little faster.
const func = function() {
    range(alpha.length-n).map(i=>{
        const r = new RegExp(alpha.slice(i,i+n).join(".*"));
        return [r,words.filter(word =>r.test(word))];
    })
        // .filter(m => m[1].length>0)
        .forEach(m=>console.log([m.toString(),m[1].length]))
}


// This works but I want to see both time together.
/*
console.time("forloop");
forloop();
console.timeEnd("forloop");

console.time("func");
forloop();
console.timeEnd("func");
*/


// Timing in millis for the different functions.
// Note: it excludes reading the wordlist etc.
// Swedish:
//   n=4: [ 521, 513 ]
//   n=5: [ 496, 487 ]
// 
// English:
//   n=4: [ 407, 402 ]
//   n=5: [ 390, 379 ]
// const t1 = timing(forloop);
// const t2 = timing(func);
// console.log([t1,t2]);

// run just one function
// const t = timing(forloop);
const t = timing(func);
console.log("time: " + t);

