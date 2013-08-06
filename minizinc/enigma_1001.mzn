% 
% Enigma puzzle 1001 in MiniZinc.
% 
% From ECLiPSe example enigma1001.pl
% """
% >From New Scientist, 17 October 1998, No 2156, p51
%
% ---------------------------------------------------
% Enigma 1001: What the hex? - by Susan Denham
% ---------------------------------------------------
%
% IN THIS hexagon of circles I've written some digits:
%
%            1   8   7
%
%          0           1
%
%        9               4
%
%          9           2
%
%            7   2   5   
%
% Reading the six sides, clockwise, as three-figure numbers you get 187,
% 714, 425, 527, 799, and 901, all of which are multiples of 17. Your
% task today is to write a new collection of non-zero digits in the
% circles, with no two adjacent digits the same, so that the six
% three-figure numbers are all different multiples of some particular
% two-figure number, the number in the top row being twice that
% two-figure number.
% """


% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

array[1..12] of var 1..9: X;
array[1..6] of var 1..99: Multipliers;
var 10..99: Divisor;

array[1..6] of var int: Numbers;

% solve satisfy;
solve :: int_search(X, first_fail, indomain_min, complete) satisfy;

constraint
        forall(i in 0..4) (
           Numbers[i+1] = 100*X[2*i+1] + 10*X[2*i+2]+X[2*i+3]
        )
        /\
	Numbers[6] = 100*X[11]+10*X[12]+X[1] /\

	% Multipler of number in top row is 2 
	Multipliers[1] = 2 /\
        forall(i in 1..6) (
          Numbers[i] = Multipliers[i]*Divisor
        )
        /\
	all_different(Multipliers) 
        /\
        forall(i in 2..12) (
           X[i] != X[i-1]
        )
        /\
        X[12] != X[1]
;

output [
        show(X), "\n\n",
	"     ", show(X[1]), "   ", show(X[2]), "   ", show(X[3]), "\n",
        "   ", show(X[12]), "           ", show(X[4]), "\n",
        " ", show(X[11]), "               ", show(X[5]), "\n",
        "   ", show(X[10]), "           ", show(X[6]), "\n",
        "      ", show(X[9]), "   ", show(X[8]), "   ", show(X[7]), "\n"


];
