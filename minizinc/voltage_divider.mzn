% 
% Voltage Divider in MiniZinc.
% 
% Problem from Marriott & Stuckey Programming in Constraints, page 137ff 
% 
% (This is also an etude in converting Prolog programs to MiniZinc...)
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


solve satisfy;

%%
%% Rules for voltage divider program page 138
%%
predicate voltage_divider(var -100.0..100.0: V, var -100.0..100.0: I, var -100.0..100.0: R1, var -100.0..100.0: R2, var -100.0..100.0: VD, var -100.0..100.0: ID) =
     let {
       var -100.0..100.0: V1,
       var -100.0..100.0: I2 
     }
     in
     V1 = I * R1
     /\
     VD = I2 * R2  
     /\ V = V1 + VD  
     /\ I = I2 + ID
;
 
% cell(9). 
% cell(12). 
array[1..2] of float: cells = [9.0, 12.0];
predicate cell(var float: c) =
  exists(i in 1..2) (
    c = cells[i]
  )
;

% resistor(5). 
% resistor(9). 
% resistor(14).
array[1..3] of float: resistors =  [ 5.0, 9.0, 14.0 ];
predicate resistor(var float: r) =
   exists(i in 1..3) (
      r = resistors[i]
   )
;


%% Goal for voltage divider program page 138.
var -100.0..100.0:  V;
var -100.0..100.0: R1;
var -100.0..100.0: R2;
var -100.0..100.0: VD;
var -100.0..100.0: ID;
var -100.0..100.0:  I;
constraint
    voltage_divider(V, I, R1, R2, VD, ID)
    /\ 
    5.4 <= VD
    /\ 
    VD <= 5.5
    /\
    ID = 0.1
    /\
    cell(V)
    /\ 
    resistor(R1)
    /\ 
    resistor(R2)
;


output [show([V,I,R1,R2,VD,ID])];