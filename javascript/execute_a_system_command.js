/* 

  Execute a system command in JavaScript.

  http://rosettacode.org/wiki/Execute_a_system_command
  """
  Task

  Run either the ls system command (dir on Windows), or the pause system command. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const utils = require('./js_utils.js');
const {execSync} = require('child_process');

const ls = execSync("ls");
console.log(ls.toString().split("\n"));
