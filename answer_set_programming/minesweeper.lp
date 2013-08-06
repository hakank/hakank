%
% Minesweeper in ASP.
%
% From gecode/examples/minesweeper.cc:
% """
% A specification is a square matrix of characters. Alphanumeric characters represent
% the number of mines adjacent to that field. Dots represent fields with an unknown number
% of mines adjacent to it (or an actual mine).
% """
% 
% E.g.
%      "..2.3."
%      "2....."
%      "..24.3"
%      "1.34.."
%      ".....3"
%      ".3.3.."
% """
% 
% Also see 
%  
% http://www.janko.at/Raetsel/Minesweeper/index.htm
%  (the first 10 examples are from)
%
% http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
%
% Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/
%
% Richard Kaye's Minesweeper Pages
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
% Some Minesweeper Configurations
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
%
%
% This encoding was inspired by the "Phase1" encoding from
% http://cse459-598-s10.wikispaces.asu.edu/Project+-+Nitisha+Desai
%
% More specific: I have removed the #domain:s and some other small changes.
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

nums(0..8).
rows(1..r).
cols(1..c).

%a cell can be a number or a mine
1 {number(R,C,Z) : nums(Z), mine(R, C)} 1 :- rows(R), cols(C).

% defining adjacency
adj(R,C,R1,C1) :- rows(R;R1), cols(C;C1), |R-R1| + |C-C1|==1.
adj(R,C,R1,C1) :- rows(R;R1), cols(C;C1), |R-R1|==1, |C-C1|==1.

% N mines around a number N
N {mine(R2, C2) : adj(R2,C2,R1,C1)} N :- number(R1,C1,N).

% #hide adj(R,C,R1,C1).
#hide.
#show mine/2.

