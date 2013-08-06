% 
% Hundred Fowls puzzle in MiniZinc.
% 
% From Formula One sample:
% http://www.f1compiler.com/samples/Hundred%20Fowls.f1.html
% """
% Hundred Fowls Puzzle
% 
% In the late fifth and early sixth century, a Chinese mathematician Qiujian
% Zhang (Chang Ch'iu-chien) published a mathematics book, and in Chapter 38 
% he posed a hundred-fowl problem as follows: 
% 
% Assume that a cock is worth 5 coins each, a hen 3 coins, and three
% chicks together 1 coin, and that one buys 100 fowls with 100 coins, then how
% many cocks, hens and chicks are there respectively?
% """

% 
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


var 0..100: cock;
var 0..100: hen;
var 0..100: chicks;

solve satisfy;

constraint
  % 5*cock + 3*hen + three_chicks/3 = 100
  3 * 5 *cock + 3 * 3 * hen + chicks = 100*3  % multiply by 3
  /\
  3 * cock + 3 * hen + 3 * chicks = 3*100
  /\ 
  chicks mod 3 = 0
      
;

output
[
  show([cock, hen, chicks])
];
