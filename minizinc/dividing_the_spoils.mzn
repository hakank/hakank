% 
% Dividing the spoils puzzle in MiniZinc.
%
%  
% From Formula One samples:
% http://www.f1compiler.com/samples/Sam%20Loyd%27s%20Dividing%20The%20Spoils.f1.html
% """
% Sam Loyd (1841-1911), America's most famous puzzle expert, presented
% the following problem:

% After gathering 770 chestnuts, the three little girls divided them up
% so that their amounts were in the same proportion as their ages.
% As often as Mary took four chestnuts, Nellie took three, and for every
% six that Mary received, Susie took seven.
% 
% How many chestnuts did each girl get?
% """

% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

var 0..1000: mary;
var 0..1000: nellie;
var 0..1000: susie;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
    3*mary = 4*nellie 
    /\
    7*mary = 6*susie 
    /\
    mary + nellie + susie = 770
;

output
[
  show([mary, nellie, susie])
];