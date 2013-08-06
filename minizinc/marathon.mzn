%
% From Xpress example
% http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
% """
% Marathon puzzle

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
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of 1..6: POS = 1..6; % Arrival positions

%     1          2        3      4          5          6
% {"Dominique","Ignace","Naren","Olivier","Philippe","Pascal"};
set of 1..6: RUNNERS = 1..6;
array[RUNNERS, POS] of var 0..1: arrive;
array[RUNNERS] of var POS: place;

constraint 
  forall (r in RUNNERS) (
     place[r] = sum(p in POS) (p*arrive[r,p])  
  )
  /\

  % One runner per position
  forall(p in POS) (
    sum(r in RUNNERS) (arrive[r,p]) = 1
  )
  /\
  % One position per runner
  forall(r in RUNNERS) (
      sum(p in POS) (arrive[r,p]) = 1
  )
  /\
  % a: Olivier not last
   arrive[4,6] = 0
  /\
  % b: Dominique, Pascal and Ignace before Naren and Olivier
  sum(p in 5..6) (arrive[1,p]+arrive[6,p]+arrive[2,p]) = 0
  /\
  sum(p in 1..3) (arrive[3,p]+arrive[4,p]) = 0
  /\
  % c: Dominique better than third
  arrive[1,1]+arrive[1,2] = 1
  /\ 
  % d: Philippe is among the first four
  sum(p in 1..4) (arrive[5,p]) = 1
  /\
  % e: Ignace neither second nor third
  arrive[2,2]+arrive[2,3] = 0
  /\ 
  % f: Pascal three places earlier than Naren
   sum(p in 4..6) (arrive[6,p]) = 0
  /\
  sum(p in 1..3) (arrive[3,p]) = 0
  /\ 
  % g: Neither Ignace nor Dominique on fourth position
  arrive[2,4]+arrive[1,4] = 0
;


solve satisfy;

output [
   if p = 1 /\ r = 1 then 
    "  Do Ig Na Ol Ph Pa\n" ++
    show(place) 
   else "" endif ++
   if p = 1 then "\n" else " " endif ++
   show(arrive[r, p])   

   | r in RUNNERS, p in POS
] ++ ["\n"];


