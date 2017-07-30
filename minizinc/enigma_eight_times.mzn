% 
% Enigma puzzle Eight times (#1317) in MiniZinc.
% 
% http://www.f1compiler.com/samples/Enigma%201317.f1.html
% """
%  Eight times
%
%  Enigma 1317 Richard England, New Scientist magazine
%
%  I invite you to find a 5-digit number consisting of five different digits 
%  (not starting with zero) which when multiplied by 8 produces another 5-digit 
%  number consisting of the other 5-digits; in addition the sum of the digits of 
%  your 5-digit number must be greater than the sum of the digits of the product.
%  
%  Which 5-digit number have you found? (Remember that the answer required is 
%  the number as it was before the multiplication.)
%
% """
% 

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
set of int: r = 0..9;
var r: x1;
var r: x2;
var r: x3;
var r: x4;
var r: x5;

var r: y1;
var r: y2;
var r: y3;
var r: y4;
var r: y5;
var 10000..99999: x = x1*10000 + x2*1000 + x3*100 + x4*10 + x5;
var 10000..99999: y = y1*10000 + y2*1000 + y3*100 + y4*10 + y5;

% array[1..n] of var r: x = [x1,x2,x3,x4,x5];
% array[1..n] of var r:y  = [y1,y2,y3,y4,y5];
% var 10000..99999: first;
% var 10000..99999: second;
% array[1..n] of var 0..9: first_a;
% array[1..n] of var 0..9: second_a;
% array[0..9] of var int: first_gcc;
% array[0..9] of var int: second_gcc;

% predicate toNum10(array[int] of var int: tal, var int: summa) =
%           let { int: len = length(tal) }
%           in
%           summa = sum(i in 1..len) (
%             ceil(pow(10.0, int2float(len-i))) * tal[i]
%           )
%           /\ forall(i in 1..len) (tal[i] >= 0)
% ;

% solve satisfy;
solve :: int_search([x1,x2,x3,x4,x5,y1,y2,y3,y4,y5], first_fail, indomain_min, complete) satisfy;

constraint

% a different solution, using more hardcore constraints :-)
%   all_different(first_a) 
%   /\ 
%   all_different(second_a) 
%   /\
%   toNum10(first_a, first)
%   /\
%   toNum10(second_a, second)
%   /\
%   second = 8 * first
%   /\
%   global_cardinality(first_a, first_gcc)
%   /\
%   global_cardinality(second_a, second_gcc)
%   /\
%   forall(i in 0..9) (
%      first_gcc[i] != second_gcc[i]
%   )
%   /\
%   sum(first_a) > sum(second_a)

    all_different([x1,x2,x3,x4,x5,y1,y2,y3,y4,y5])
    /\
    x1 > 0  /\ y1 > 0 
    /\
    x*8 = y
    /\
    (x1 + x2 + x3 + x4 + x5) > (y1 + y2 + y3 + y4 + y5)
;

output
[
  show([x1,x2,x3,x4,x5,y1,y2,y3,y4,y5])

];
