/* 

  Letter frequency in JavaScript.

  http://rosettacode.org/wiki/Letter_frequency
  """
  Task

  Open a text file and count the occurrences of each letter.

  Some of these programs count all characters (including punctuation), 
  but some only count letters A to Z. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {collect2} = require('./js_utils.js');
const fs    = require('fs');

// Count the frequency of lower cased words and print them
// in increasing order.
function concordance_words(file) {
    const words = fs.readFileSync(file,"utf8").split(/\W+/gsm).map(w=>w.toLowerCase()).collect2();
    for(const w of Object.entries(words).sort((a,b)=>b[1]-a[1])) {
        console.log(w);
    }
    
}

// Count the frequency of lower cased chars and print them
// in increasing order.
function concordance_letters(file) {
    const words = fs.readFileSync(file,"utf8").split("").map(w=>w.toLowerCase()).collect2();
    for(const w of Object.entries(words).sort((a,b)=>b[1]-a[1])) {
        console.log(w);
    }
}

// Letters
// concordance_letters("letter_frequency.js");
// concordance_letters("js_utils.js");
concordance_letters("unixdict.txt"); // 76ms
// concordance_letters("words_lower.txt"); // a much larger wordlist 415834 words (0.4s)

// Words
// concordance_words("letter_frequency.js");
// concordance_words("js_utils.js");
