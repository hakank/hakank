% Transportation Model in Minzinc
%
% Problem and model from ECLiPSe documentation
% http://eclipse.crosscoreop.com/reports/ECLiPSe_CP2007.pdf, page 7 and 11
% (and http://eclipse.crosscoreop.com/reports/eclipse/node19.html )
%
% For a general version see transportation2.mzn
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var int: A1;
var int: A2;
var int: A3;
var int: B1;
var int: B2;
var int: B3;
var int: C1;
var int: C2;
var int: C3;
var int: D1;
var int: D2;
var int: D3;

array[1..12] of var int: Vars = [A1,A2,A3,B1,B2,B3,C1,C2,C3,D1,D2,D3];
var int: Obj;

solve :: int_search(Vars, first_fail, indomain_split, complete)  minimize Obj;
% solve minimize Obj;



constraint
    Obj >= 0 /\
    forall(i in 1..12) (
      Vars[i] >= 0
    )
    /\
    A1 + A2 + A3 = 200 /\
    B1 + B2 + B3 = 400 /\
    C1 + C2 + C3 = 300 /\
    D1 + D2 + D3 = 100 /\

    A1 + B1 + C1 + D1 <= 500 /\
    A2 + B2 + C2 + D2 <= 300 /\
    A3 + B3 + C3 + D3 <= 400 /\

    Obj = 
      10*A1 + 7*A2 + 11*A3 +
       8*B1 + 5*B2 + 10*B3+
       5*C1 + 5*C2 + 8*C3+
       9*D1 + 3*D2 + 7*D3
;

output [
  "Vars: " ++ show(Vars) ++ "\n" ++
  "Obj: " ++ show(Obj)
];