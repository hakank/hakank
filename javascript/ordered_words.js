/* 

  Ordered words in JavaScript.

  http://rosettacode.org/wiki/Ordered_words
  """
  An ordered word is a word in which the letters appear in alphabetic order.

  Examples include   abbey   and   dirt.

  Task

  Find and display all the ordered words in the dictionary unixdict.txt
  that have the longest word length.

  (Examples that access the dictionary file locally assume that you 
  have downloaded this file yourself.)

  The display needs to be shown on this page. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const utils = require('./js_utils.js');
const fs = require("fs");

function ordered_words(wordlist) {
    const words = fs.readFileSync(wordlist,"utf8").split("\n");
    const sorted = words
        .filter(word=>
                word == word.split("").sort().join(""))
        .map(word=>[word.length,word])
          .sort((a,b)=>b[0]-a[0]);
    const maxVal = sorted[0][0];
    return sorted.filter(w=>w[0]===maxVal).map(w=>w[1]);
        
}


console.log(ordered_words("unixdict.txt"));
// console.log(ordered_words("/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt"));
// console.log(ordered_words("words_lower.txt"));
