% 
% Prime multiplication in MiniZinc
%
% Martin Gardner:
% """
% Find primes (2, 3, 5 or 7) for each P such that this multiplication is correct: 
%       PPP  
%    *   PP 
% --------- 
%      PPPP 
%     PPPP 
% --------- 
%     PPPPP
% """
%
 
% Representation of the digits:
% 
%       ABC
%    *   DE 
% --------- 
%      FGHI
%     JKLM_
% --------- 
%     NOPQR
%


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

set of int: primes = {2,3,5,7};

% decision variables

var int: abc;
array[1..3] of var primes: abc_a;
var int: de;
array[1..2] of var primes: de_a;
var int: fghi;
array[1..4] of var primes: fghi_a;
var int: jklm;
array[1..4] of var primes: jklm_a;
var int: nopqr;
array[1..5] of var primes: nopqr_a;

% Requires MiniZinc 2
function var int: to_num(array[int] of var int: a) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(10, len-i) * a[i] 
                 );
         } in n
;

solve satisfy;

constraint

  % preparation
  abc   = to_num(abc_a) /\
  de    = to_num(de_a) /\
  fghi  = to_num(fghi_a) /\
  jklm  = to_num(jklm_a) /\
  nopqr = to_num(nopqr_a) /\
 
  % the equation
  abc * de_a[2] = fghi /\
  abc * de_a[1] = jklm /\
  fghi + 10*jklm =  nopqr
  % /\ abc * de = nopqr

;

output [

  "abc: ", show(abc), "\n",
  "de: ", show(de), "\n",
  "fghi: ", show(fghi), "\n",
  "jklm: ", show(jklm), "\n",
  "nopqr: ", show(nopqr), "\n",
   "\n\n",
   "   ", show(abc), "\n",
   "*   ", show(de), "\n",
   "------\n",
   "  ", show(fghi), "\n",
   "+", show(jklm), " \n",
   "------\n",
   " ", show(nopqr), " \n",

];

