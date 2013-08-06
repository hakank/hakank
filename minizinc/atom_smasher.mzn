% 
% The Digital Atom Smasher (alphametic problem) in MiniZinc.
% 
% From http://www.math.uni-bielefeld.de/~sillke/PUZZLES/ALPHAMETIC/
% """
% sqrt(ATOM)  =  A + TO + M
% 
% this has two solutions
% Ref: Angela Fox Dunn, Mathematical Bafflers, 1980, p39 
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

var 0..9: A;
var 0..9: T;
var 0..9: O;
var 0..9: M;
var int: ATOM = 1000*A+100*T+10*O+M;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   let {
     var 0..1000: z
   }
   in
   z * z = ATOM
   /\
   z = A + (10*T+O) + M
   /\
   all_different([A,T,O,M])
;

output [
 "ATOM = A + TO + M","\n",
 show(ATOM), " = ", show(A), " + ", show(10*T+O), " + ", show(M), "\n",

];