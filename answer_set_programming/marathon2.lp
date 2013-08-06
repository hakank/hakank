% 
% Marathon puzzle in ASP.
% 
% From Xpress example
% http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
% """
% Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
% have arrived as the first six at the Paris marathon.
% Reconstruct their arrival order from the following
% information:
% a) Olivier has not arrived last
% b) Dominique, Pascal and Ignace have arrived before Naren
%    and Olivier
% c) Dominique who was third last year has improved this year.
% d) Philippe is among the first four.
% e) Ignace has arrived neither in second nor third position.
% f) Pascal has beaten Naren by three positions.
% g) Neither Ignace nor Dominique are on the fourth position.
% 
%    (c) 2002 Dash Associates
%   author: S. Heipcke, Mar. 2002
% """
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% Solution:
% Dominique, Ignace, Naren, Olivier, Philippe, Pascal
% 2,         1,      6,     5,       4,        3

#const n = 6.
runners(dominique;ignace;naren;olivier;philippe;pascal).

% positions
positions(1..n).


% alldifferent positions
1 { pos(Runner, Position) : positions(Position) } 1 :- runners(Runner).
1 { pos(Runner, Position) : runners(Runner) } 1 :- positions(Position).

% a: Olivier not last
:- pos(olivier,n).

% b: Dominique, Pascal and Ignace before Naren and Olivier
before(R1, R2) :- positions(P1;P2), runners(R1;R2), pos(R1, P1), pos(R2, P2), P1 < P2.

:- not before(dominique, naren).
:- not before(dominique, olivier).
:- not before(pascal, naren).
:- not before(pascal, olivier).
:- not before(ignace, naren).
:- not before(ignace, olivier).

% c: Dominique better than third
:- pos(dominique, Pos), Pos >= 3.

% d: Philippe is among the first four
:- pos(philippe, Pos), Pos > 4.

% e: Ignace neither second nor third
:- pos(ignace, Pos),Pos = 2.
:- pos(ignace, Pos), Pos = 3.

% f: Pascal three places earlier than Naren
:- pos(pascal, Pascal), pos(naren, Naren), Pascal + 3 != Naren.

% g: Neither Ignace nor Dominique on fourth position
:- pos(ignace, Pos), Pos = 4.
:- pos(dominique, Pos), Pos = 4.


#hide.
#show pos(Runner, Position).