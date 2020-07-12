/*
  Read test in JavaScript.

  This is one of my standard test when learning a new programming
  language: read a word file and filters the words that match 
  the regular expression
      a.*b.*c..., b.*c.*d.., c.*d.*e..., etc


  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/


*/
'use strict';
const {range,timing} = require("./js_utils.js");

var fs = require("fs");
var lang = "eng";
var wordlist = "words_lower.txt";
var alpha = "abcdefghijklmnopqrstuvxyz".split(""); // English (etc) words
var n = 5;

if (lang === "swe") {
    wordlist = "/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt";
    alpha = "abcdefghijklmnopqrstuvxyzåäö".split(""); // Swedish words    
}

var words = fs.readFileSync(wordlist).toString().split("\n");
console.log("number of words: " + words.length);

//
// for loop with some functional ingredients
//
var forloop = function() {

    for (var i = 0; i < alpha.length - n; i++) {
        var r = new RegExp(alpha.slice(i,i+n).join(".*"));
        console.log("\t" + r);
        var m = words.filter(word =>r.test(word));
        // if (m.length > 0) {
        console.log(m.toString());
        console.log(" len: " + m.length + "\n");
        // }
    }
}

// A more functional approach, a little faster.
var func = function() {
    range(alpha.length-n).map(i=>{
        var r = new RegExp(alpha.slice(i,i+n).join(".*"));
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
// var t1 = timing(forloop);
// var t2 = timing(func);
// console.log([t1,t2]);

// run just one function
// var t = timing(forloop);
var t = timing(func);
console.log("time: " + t);

