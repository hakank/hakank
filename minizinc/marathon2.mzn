%
% Marathon puzzle in MiniZinc.
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
% Compare with the linear programming model
% http://www.hakank.org/minizinc/marathon.mzn
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Solution: [2, 1, 6, 5, 4, 3]
%

include "globals.mzn";
int: n = 6;
var 1..n: Dominique;
var 1..n: Ignace;
var 1..n: Naren;
var 1..n: Olivier;
var 1..n: Philippe;
var 1..n: Pascal;

% decision variables
array[1..n] of var 1..n: runners = [Dominique, Ignace, Naren, Olivier, Philippe, Pascal];
array[1..n] of var 1..n: runners_pres; % for presentation

solve satisfy;

constraint 
  all_different(runners) /\
  inverse(runners, runners_pres) /\
  
  % a: Olivier not last
  Olivier    != n /\

  % b: Dominique, Pascal and Ignace before Naren and Olivier
  Dominique  < Naren /\
  Dominique  < Olivier /\
  Pascal     < Naren /\
  Pascal     < Olivier /\
  Ignace     < Naren /\
  Ignace     < Olivier /\

  % c: Dominique better than third
  Dominique  < 3 /\ 

  % d: Philippe is among the first four
  Philippe   <= 4  /\

  % e: Ignace neither second nor third
  Ignace     != 2 /\ 
  Ignace     != 3 /\ 

  % f: Pascal three places earlier than Naren
  Pascal + 3 = Naren /\ 

  % g: Neither Ignace nor Dominique on fourth position
  Ignace     != 4 /\
  Dominique  != 4
;

output
[
  show(runners) ++ "\n" ++
  show(runners_pres) ++ "\n" 
];
