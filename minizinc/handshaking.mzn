% 
% Halmos' handshake problem in MiniZinc.
% 
% Problem formulation from Alloy (examples/puzzles/handshake)
% """
% Alloy model of the Halmos handshake problem
% 
% Hilary and Jocelyn are married. They invite four couples who are friends for dinner. When
% they arrive, they shake hands with each other. Nobody shakes hands with him or herself
% or with his or her spouse. After there has been some handshaking, Jocelyn jumps up on
% a chair and says "Stop shaking hands!", and then asks how many hands each person has
% shaken. All the answers are different. How many hands has Hilary shaken?
% 
% The Alloy model represents the problem as a set of constraints. Properties of the spouse
% relationship and of handshaking in general are given as facts. The particular situation
% is cast as a function.
% 
% There are 9 people answering, and all answers are different. Nobody can shake more than
% 8 hands. So answers must be 0..8. The one (p8 say) who answered 8 has shaken everybody's
% hand except for his or her own, and his or her spouse's. Now consider the person who shook
% 0 hands (p0 say). The persons p0 and p8 are distinct. If they are not married, then p8 cannot
% have shaken 8 hands, because he or she did not shake the hand of p0 or of his or her spouse.
% So p8's spouse to p0. Now imagine Jocelyn asking the question again, with p0 and p8 out of
% the room, and excluding hand shakes with them. Since p8 shook hands with everyone else
% except p0 and p8, everyone gives an answer one smaller than they did before, giving 0..6.
% The argument now applies recursively. So Hilary is left alone, having shaken 4 hands. 
% """
% Alloy is here: http://alloy.mit.edu/alloy
% 
% Also, see the following that discuss Halmos' Handshake problem
% http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/mathhome.htm#halmos
%     http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/shakeanswer.htm
% 
% The origin of the problem seems to be
% P.R. Halmos: "To Count or to Think, That is the Question", page 1ff
% http://bernoulli.math.rug.nl/vorigelezingen/lezing03/lezing03.pdf
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 100; % 5 pairs

% decision variables
%   coded Pair1a,Pair1b, Pair2a,Pair2b
array[1..n] of var 0..n-2: x; % can shake max n-2 hands

% who shake hands with whom:
%  (restrictions: not him/herself and not his/her spouse)
array[1..n, 1..n] of var 0..1: y;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_median, complete) satisfy;

constraint
  % We assume that Hilary is in position x[1]
  % (and Hilary's spouse - Jocelyn - in x[2])
  % All except Hilary's counts are different
  alldifferent([x[i] | i in 2..n])
  /\
  forall(i in 0..(n div 2)-1) (
    % don't shake hand with spouse
    y[2*i+1,2*i+2] = 0
    /\
    y[2*i+2,2*i+1] = 0 
  )
  /\
  forall(i in 1..n) (
   % don't shake hand with oneself
    y[i,i] = 0
 
    % how many hands has x[i] shaken
    /\ x[i] = sum([y[i,j] | j in 1..n]) 
  )
  /\
  forall(i, j in 1..n) (
    % symmetry of handshaking:
    %    a shake hands with b <-> b shake hands with a
    y[i,j] = 1 <-> y[j,i] = 1
  )

  /\ % symmetry breaking which orders the other couples (besides the hosts)
     % Without it: 384 solutions (all x = [4,4,.....])
     % With it: 1 solution: x: [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] (since we order 0,1,2,3 shakes)
     % 
     % Note that all number of handshaking of the pairs sums to 8, i.e. 4+4, 0+8, 1+7, 2+6, 3+5
     % More general: The number of handshaking per pair sums to n-2.
     % 
  increasing([x[3+2*i] | i in 0..(n div 2)-2])
  /\
  forall(i in 0..(n div 2)-2) (
    x[3+2*i] < x[3+2*i+1]
  )

  % /\ x[1] != 4 % /\ x[2] != 4 % just for testing contradictions
;

output 
[ "x: " ++ show(x) ++ "\n\nWho shake hands with whom:" ] ++
[
  if j = 1 then "\n" else " " endif ++
    show(y[i,j])
  ++
  if j = n then " = " ++ show(x[i]) else "" endif
  | i, j in 1..n
];
