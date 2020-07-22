/* 

  Character codes in JavaScript.

  http://rosettacode.org/wiki/Character_codes
  """
  Task

  Given a character value in your language, print its code 
  (could be ASCII code, Unicode code, or whatever your language uses).


  Example

  The character 'a' (lowercase letter A) has a code of 97 in ASCII
  (as well as Unicode, as ASCII forms the beginning of Unicode).

  Conversely, given a code, print out the corresponding character
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const utils = require('./js_utils.js');

console.log("a");
console.log("a".charCodeAt(0));
console.log(String.fromCharCode(97));

console.log(String.fromCharCode(72, 69, 76, 76, 79));


console.log("håkan".split("").map(c=>c.charCodeAt()));
