/* 

  Environment variables in JavaScript.

  http://rosettacode.org/wiki/Environment_variables
  """
  Task

  Show how to get one of your process's environment variables.

  The available variables vary by system; some of the common ones available on Unix include:

  - PATH
  - HOME
  - USER
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const utils = require('./js_utils.js');

const process = require("process");

console.log(["PATH","HOME","USER"].map(v=>[v,process.env[v]]));
