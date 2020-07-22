/* 

  Text processing/Max licenses in use in JavaScript.

  http://rosettacode.org/wiki/Text_processing/Max_licenses_in_use
  """
  A company currently pays a fixed sum for the use of a particular 
  licensed software package. In determining if it has a good deal it 
  decides to calculate its maximum use of the software from its 
  license management log file.

  Assume the software's licensing daemon faithfully records a checkout 
  event when a copy of the software starts and a checkin event when 
  the software finishes to its log file.

  An example of checkout and checkin events are:

    License OUT @ 2008/10/03_23:51:05 for job 4974
    ...
    License IN  @ 2008/10/04_00:18:22 for job 4974

  Task

  Save the 10,000 line log file from 
     https://rosettacode.org/resources/mlijobs.txt
     [note: file not found!]

  into a local file, then write a program to scan the file extracting 
  both the maximum licenses that were out at any time, and the time(s) 
  at which this occurs.

  Mirror of log file available as a zip 
    https://github.com/thundergnat/rc/blob/master/resouces/mlijobs.zip
  (offsite mirror). 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {scan,argmaxes,max2} = require('./js_utils.js');

const {readFileSync} = require("fs");

const file = "mlijobs.txt";

function max_licenses1() {

    let h = {};
    let count = 0;
    const entries = readFileSync(file,{encoding:"utf8"}).toString("").split("\n");
    for(const entry of entries) {
        const s = entry.match(/^License\s+(OUT|IN)\s+@\s+(.+?)\s+for job\s+(\d+)$/);
        const flow = s[1];
        const date = s[2];
        const job = s[3];
        if (!h[date]) {
            h[date] = 0;
        }
        if (flow === 'OUT') {
            count++;
            h[date] = count;
        } else if (flow === 'IN') {
            count--;
            h[date] = count;        
        } else {
            throw(Error("BAD FLOW: ", flow));
        }
    }
    
    const sorted = Object.entries(h)
          .sort((a,b)=>b[1]-a[1]);
    
    const maxval = sorted[0][1];
    console.log("MaxVal:", maxval);
    console.log(Object.entries(h).filter(e=>e[1]===maxval).map(e=>e[0]));

}


// Nicer approach, a little more functional
function max_licenses2() {
    const entries = readFileSync(file,{encoding:"utf8"}).toString("").split("\n");
    // Keep an accumulated sum of the entries
    // const acc = asum(entries
    //                .map(e=>e.slice(8,11) === "OUT" ? 1 : -1));
    const acc = scan(entries
                     .map(e=>e.slice(8,11) === "OUT" ? 1 : -1),
                     (a,b) => a+b);
    console.log("MaxVal:", acc.max2());
    // and then we pick the max values to show the dates
    console.log([...argmaxes(acc).map(i=>entries[i].slice(14,33))]);    
}

max_licenses1();
max_licenses2();
