/* 

  Count occurrences of a substring in JavaScript.

  http://rosettacode.org/wiki/Count_occurrences_of_a_substring
  """
  Task

  Create a function, or show a built-in function, to count the number 
  of non-overlapping occurrences of a substring inside a string.

  The function should take two arguments:

  - the first argument being the string to search, and
  - the second a substring to be searched for. 

  It should return an integer count.

  print countSubstring("the three truths","th")
  3
 
  // do not count substrings that overlap with previously-counted 
  // substrings:
  print countSubstring("ababababab","abab")
  2

  The matching should yield the highest number of non-overlapping 
  matches.

  In general, this essentially means matching from left-to-right or 
  right-to-left (see proof on talk page).


  Metrics: length
  Sub-string search: Count occurrences of a substring
  Multi-string operations: LCP, LCS, concatenation
  Manipulation: reverse, lower- and uppercase
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const utils = require('./js_utils.js');


// This is nice but it don't handle the "a.*b" below correctly
// without escaping the magical regexp characters.
function countSubstring(s,ss) {
    ss = escapeRegExp(ss);
    const res = s.match(new RegExp(ss,"g"));
    return res.length;
}

// Translated from the C++ version.
function countSubstring2(s,ss) {
    if (ss.length === 0) {
        return 0;
    }
    let count = 0;
    for (let offset = s.indexOf(ss);
         offset < s.length && offset >= 0;
	 offset = s.indexOf(ss, offset + ss.length)) {
        count++;
    }
    return count;
}

// A littler nicer version than the C++ approach
function countSubstring3(s, ss) {
    let idx = 0;
    let count = 0;
    const sslen = ss.length;
    while (true) {
        idx = s.indexOf(ss, idx);
        if (idx == -1) {
            break;
        }
        idx += sslen;
        count++;
    }
    return count
}
 

// From https://stackoverflow.com/questions/3115150/how-to-escape-regular-expression-special-characters-using-javascript
function escapeRegExp(text) {
  return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&'); // 
}

console.log("countSubstring:");
console.log(countSubstring("the three truths","th"));
console.log(countSubstring("ababababab","abab"));
console.log(countSubstring("abaabba*bbaba*bbab", "a*b"));
console.log(countSubstring("abaabba*bbaba*bbab", "a"));

console.log("countSubstring2:");
console.log(countSubstring2("the three truths","th"));
console.log(countSubstring2("ababababab","abab"));
console.log(countSubstring2("abaabba*bbaba*bbab", "a*b"));
console.log(countSubstring2("abaabba*bbaba*bbab", "a"));

console.log("countSubstring3:");
console.log(countSubstring3("the three truths","th"));
console.log(countSubstring3("ababababab","abab"));
console.log(countSubstring3("abaabba*bbaba*bbab", "a*b"));
console.log(countSubstring3("abaabba*bbaba*bbab", "a"));
