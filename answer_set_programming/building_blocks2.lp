% 
% Building Blocks puzzle (Dell Logic Puzzles) in ASP.
% 
% From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
%   """
%   Each of four alphabet blocks has a single letter of the alphabet on each 
%   of its six sides. In all, the four blocks contain every letter but 
%   Q and Z. By arranging the blocks in various ways, you can spell all of 
%   the words listed below. Can you figure out how the letters are arranged 
%   on the four blocks?
%
%   BAKE ONYX ECHO OVAL
%
%   GIRD SMUG JUMP TORN
%
%   LUCK VINY LUSH WRAP
%   """
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% Here is one (of 24) solution:
% a,2 b,1 c,2 d,2 e,4 f,1 g,4 h,3
% i,1 j,2 k,3 l,4 m,3 n,2 o,1 p,4
% r,3 s,2 t,4 u,1 v,3 w,1 x,3 y,4
% 
% With the symmetry breaking of placing the first word (bake)
% we get a single solution.

word(1, b;a;k;e).
word(2, o;n;y;x).
word(3, e;c;h;o).
word(4, o;v;a;l).
word(5, g;i;r;d).
word(6, s;m;u;g).
word(7, j;u;m;p).
word(8, t;o;r;n).
word(9, l;u;c;k).
word(10, v;i;n;y).
word(11, l;u;s;h).
word(12, w;r;a;p).

words(1..12).
letters(a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;r;s;t;u;v;w;x;y).
d(1..4).

1 { letter(Letter, Dice) : d(Dice) } 1 :- letters(Letter).
6 { letter(Letter, Dice) : letters(Letter) } 6 :- d(Dice).

% Symmetry breaking (reducing from 24 to 6 solutions)
% letter(a,1).

% Symmetry breaking: first word is placed. (24 solutions -> 1 solution.)
letter(b,1).
letter(a,2).
letter(k,3).
letter(e,4).

% :- words(W),
%     word(W, L1),
%     word(W, L2),
%     L1 < L2, 
%     letter(L1,D1), 
%     letter(L2,D2),
%     D1==D2.

:- words(W),
   word(W, L1),
   word(W, L2),
   L1 < L2, 
   letter(L1,D1), 
   letter(L2,D1).

#hide.
#show letter(Letter, Dice).