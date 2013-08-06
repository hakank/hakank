%
% Five brigades puzzle in Minizinc.
%
% From http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
% """
% The Five Brigands    from "Amusements in Mathematics, Dudeney", number
% 133.

% The five Spanish brigands, Alfonso, Benito, Carlos, Diego, and Esteban,
% were counting their spoils after a raid, when it was found that they
% had captured altogether exacly 200 doubloons. One of the band pointed
% out that if Alfonso had twelve times as much, Benito three times as
% much, Carlos the same amount, Diego half as much, and Esteban one-
% third as much, they would still have altogether just 200 doubloons.
% How many doubloons had each?

% There are a good many equally correct answers to this problem. The
% puzzle is to discover exactly how many different answers there are, it
% being understood that every man had something and there is to be no
% fractional money. 
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Everybody has at least 8d.; nobody has more than 160
set of int: xrange = 8..160;
var xrange: A;
var xrange: B;
var xrange: C;
var 8..100: D2;
var 8..66: E3;

array[1..5] of  var int: LD = [A,B,C,D2,E3];

solve :: int_search(LD, first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
  A + B + C + 2*D2 + 3*E3  = 200 /\
  A * 12 + B * 3 + C + D2 + E3  = 200
;


output
[
  show(LD)
];
