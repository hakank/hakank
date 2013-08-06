% 
% Cryptarithmetic puzzle in MiniZinc.
% 
% Prolog benchmark problem GNU Prolog (crypta.pl)
% """
% Name           : crypta.pl                                              
% Title          : crypt-arithmetic                                       
% Original Source: P. Van Hentenryck's book                               
% Adapted by     : Daniel Diaz - INRIA France                             
% Date           : September 1992                                         
%                                                                        
% Solve the operation:                                                    
%                                                                       
%    B A I J J A J I I A H F C F E B B J E A                              
%  + D H F G A B C D I D B I F F A G F E J E                              
%  -----------------------------------------                              
%  = G J E G A C D D H F A F J B F I H E E F                              
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 0..9;
var r: A;
var r: B;
var r: C;
var r: D;
var r: E;
var r: F;
var r: G;
var r: H;
var r: I;
var r: J;
array[1..10] of var r: LD = [A,B,C,D,E,F,G,H,I,J];

var 0..1: Sr1;
var 0..1: Sr2;


% solve satisfy;
solve :: int_search(LD, first_fail, indomain_min, complete) satisfy;

constraint
           B >= 1
           /\
           D >= 1
           /\
           G >= 1
           /\
         
	   A+10*E+100*J+1000*B+10000*B+100000*E+1000000*F+
	   E+10*J+100*E+1000*F+10000*G+100000*A+1000000*F
	= F+10*E+100*E+1000*H+10000*I+100000*F+1000000*B+10000000*Sr1
           /\
      
	   C+10*F+100*H+1000*A+10000*I+100000*I+1000000*J+
	   F+10*I+100*B+1000*D+10000*I+100000*D+1000000*C+Sr1
	= J+10*F+100*A+1000*F+10000*H+100000*D+1000000*D+10000000*Sr2
         /\

	   A+10*J+100*J+1000*I+10000*A+100000*B+
	   B+10*A+100*G+1000*F+10000*H+100000*D+Sr2
	= C+10*A+100*G+1000*E+10000*J+100000*G
        /\
        all_different(LD)

;

output [
  show(LD), "\n",

];
