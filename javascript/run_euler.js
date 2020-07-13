/* 

  Run Euler problems in JavaScript.

  Note: This program don't do any checks etc, just running (loading) 
        the euler*.js programs.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2} = require('./js_utils.js');

range2(1,50)
    .forEach(n=> {
        const euler = require(`./euler${n}.js`);
    })

