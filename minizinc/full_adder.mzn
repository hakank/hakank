% 
% Full Adder in MiniZinc.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var bool: I1;
var bool: I2;
var bool: I3;
var bool: O1;
var bool: O2;


predicate fullAdder(var bool:I1,
                    var bool:I2,
                    var bool:I3,
                    var bool:O1,
                    var bool:O2) =
   let {
     var bool: X1,
     var bool: Y1,
     var bool: Y2
   }
   in
   X1 = 'xor'(I1,I2)  /\
   Y1 = '/\'(I1,I2)   /\
   O1 =  'xor'(X1,I3) /\
   Y2 ='/\'(I3,X1)    /\
   O2 = '\/'(Y1,Y2)
;

solve satisfy;

constraint

  I3 = false
  /\ 
  O2 = true
  /\
  fullAdder(I1,I2,I3,O1,O2)

;

output [
  "[I1,I2,I3,O1,O2]: ", show([I1,I2,I3,O1,O2]), "\n"

];