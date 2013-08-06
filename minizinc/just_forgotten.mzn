% 
% Just forgotten puzzle (Enigma 1517)  in MiniZinc.
% 
% From http://www.f1compiler.com/samples/Enigma%201517.f1.html
% """
%
% Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
%
% 
% Joe was furious when he forgot one of his bank account numbers. 
% He remembered that it had all the digits 0 to 9 in some order, so he tried
% the following four sets without success:
%
%     9 4 6 2 1 5 7 8 3 0
%     8 6 0 4 3 9 1 2 5 7 
%     1 6 4 0 2 9 7 8 5 3
%     6 8 2 4 3 1 9 0 7 5
%
% When Joe finally remembered his account number, he realised that in each set
% just four of the digits were in their correct position and that, if one knew
% that, it was possible to work out his account number.
% What was it?
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:%www.hakank.org/minizinc
%

include "globals.mzn"; 

int: rows = 4;
int: cols = 10;

array[1..rows, 1..cols] of var 0..9: a;
array[1..10] of var 0..9: x;

solve satisfy;

constraint
   all_different(x)
   /\
   % compared to the F1 model, this is slightly simpler
   forall(r in 1..rows) (
     sum(c in 1..cols) (
        bool2int(x[c] = a[r,c])
     ) = 4
  )
;

a = array2d(1..rows, 1..cols, 
  [9,4,6,2,1,5,7,8,3,0,
   8,6,0,4,3,9,1,2,5,7,
   1,6,4,0,2,9,7,8,5,3,
   6,8,2,4,3,1,9,0,7,5]);


output
[
  "x: " ++ show(x)
];
