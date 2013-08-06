% 
% Unknown multiplication problem in MiniZinc.
% 
% Standard Prolog benchmark (GNU Prolog)
% """
%  Name           : multipl.pl                                             
%  Title          : unknown multiplication                                 
%  Original Source: Daniel Diaz - INRIA France                             
%  Adapted by     : Daniel Diaz for GNU Prolog                             
%  Date           : June 1995                                              
%                                                                          
%  Find the value of each digit verifying the following multiplication and 
%  such that each digit (0,1,...,9) appears excatly twice:                 
%                                                                          
%                    X1  X2  X3                                            
%                  * X4  X5  X6                                            
%                   -----------                                            
%                    X7  X8  X9                                            
%         +      X10 X11 X12                                               
%         + X13 X14 X15                                                    
%         = -------------------                                            
%           X16 X17 X18 X19 X20                                            
% 
% """

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 0..9;
var r: X1;
var r: X2;
var r: X3;
var r: X4;
var r: X5;
var r: X6;
var r: X7;
var r: X8;
var r: X9;
var r: X10;
var r: X11;
var r: X12;
var r: X13;
var r: X14;
var r: X15;
var r: X16;
var r: X17;
var r: X18;
var r: X19;
var r: X20;

array[1..20] of var r:  LD=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,
                            X11,X12,X13,X14,X15,X16,X17,X18,X19,X20];

var 0..1000: Y;
var 0..1000: Z1;
var 0..1000: Z2;
var 0..1000: Z3;

% solve satisfy;
solve :: int_search(LD, input_order, indomain_max, complete) satisfy;

constraint
    
    % note: we use array1d since the array starts at index 0
    global_cardinality(LD, array1d(0..9, set2array(0..9)), array1d(0..9,[2,2,2,2,2,2,2,2,2,2])) % :: domain


    % forall(i in 0..9) (
    %   exactly(2, LD, i)
    % )

     /\ 

    Y=100*X1+10*X2+X3 /\ 

    Z1 = 100*X7 +10*X8 +X9 /\ 
    Z2 = 100*X10+10*X11+X12 /\ 
    Z3 = 100*X13+10*X14+X15 /\ 
    
    X6*Y  =  Z1 /\ 
    X5*Y  =  Z2 /\ 
    X4*Y  =  Z3 /\ 

    100*X7 +10*X8 +X9 + 
    1000*X10+100*X11+10*X12    + 
    10000*X13+1000*X14+100*X15          = 
    10000*X16+1000*X17+100*X18+10*X19+X20
;


output
[
  show(LD)
];