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
% Note: It takes 1min:10 seconds for gringo/clasp to generate
% all 24 solutions.
% The grounding of gringo takes a quite much of this time: 30 seconds.
%
% For lparse/smodels I skipped after 50 minutes when just 6 solutions
% had been shown.

word(b,a,k,e).
word(o,n,y,x).
word(e,c,h,o).
word(o,v,a,l).
word(g,i,r,d).
word(s,m,u,g).
word(j,u,m,p).
word(t,o,r,n).
word(l,u,c,k).
word(v,i,n,y).
word(l,u,s,h).
word(w,r,a,p).


letters(a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;r;s;t;u;v;w;x;y).
d(1..4).

1 { letter(Letter, Dice) : d(Dice) } 1 :- letters(Letter).
6 { letter(Letter, Dice) : letters(Letter) } 6 :- d(Dice).

% the letters of each word must be on
% difference dice
diff(L1,L2,L3,L4) :-
    letters(L1;L2;L3;L4),d(D1;D2;D3;D4), % for lparse
    letter(L1,D1), 
    letter(L2,D2), 
    letter(L3,D3), 
    letter(L4,D4),
    D1 != D2,
    D1 != D3,
    D1 != D4,
    D2 != D3,
    D2 != D4,
    D3 != D4.
:- not diff(L1,L2,L3,L4), word(L1,L2,L3,L4).

#hide.
#show letter(Letter, Dice).