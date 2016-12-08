% 
% Scene allocation problem in MiniZinc.
% 
% From 
% Pascal Van Hentenryck
% "Constraint and Integer Programming in OPL"
% page 362 (or 18 depending how you count)
% """
% The scene-allocation problem was communicated to us by Irvin Lustig, 
% who also provided an integer-programming model and a specific instance. 
% It consists of deciding when to shoot scenes for a movie. Each scene 
% invoves a number of actors and at most 5 scenes a day can be filmed. 
% All actors of a scene must, of course, be present on the day the scene 
% is shot. The actors have fees representing the amount to be paid per day 
% they spent in the studio. The goal of the application is to minimize 
% the production costs.
% """

% Also, see http://www.drdobbs.com/sticks/184404013
% Dennis E. Shasha
% http://cs.nyu.edu/shasha/papers/acks
% http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/2000/0004/0004r/0004r.htm
% http://collaboration.cmc.ec.gc.ca/science/rpn/biblio/ddj/Website/articles/DDJ/2000/0001/0001n/0001n.htm
% Note: The answer given in http://www.drdobbs.com/sticks/184404013
% """
% This gives a final price of $3,517,350.00.
% """
% Is not the one this model gives: 3341440



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: maxScene;
array[1..maxScene] of set of int: appears;
int: maxDay;
int: numActors;

array[1..numActors] of int: Actor;
array[1..numActors] of string: ActorS;
array[1..numActors] of int: pay;

% which scenes are actor a in
array[1..numActors] of set of int: which = 
    [ {s | s in 1..maxScene where a in appears[s] } | a in 1..numActors];


%
% decision variables
%
array[1..maxScene] of var 1..maxDay: shoot;
array[1..numActors, 1..maxDay] of var 0..1: isPaid;
array[1..maxScene, 1..maxDay] of var 0..1: isShot;

% var int: cost = sum(a in 1..numActors, d in 1..maxDay) (
%     pay[a] * bool2int(exists(s in which[a]) (isShot[s,d] = 1))
%   );  
% var int: cost = sum(a in 1..numActors) (
%         pay[a] * sum(d in 1..maxDay) ( bool2int(exists(s in which[a]) (shoot[s] = d)))
%   );  

% quite good
% var int: cost = sum(a in 1..numActors, d in 1..maxDay) (
%     pay[a] * bool2int(exists(s in which[a]) (shoot[s] = d))
%   );  

% better
var int: cost = sum(a in 1..numActors, d in 1..maxDay) ( pay[a] * isPaid[a,d]);

% Comet (0-based): 334144,  shoot[2,3,0,1,3,2,1,3,0,0,1,3,1,2,3,0,0,2,1]

% solve satisfy;
% solve minimize cost;
solve :: int_search(shoot, first_fail, indomain_max, complete) minimize cost;
% ann: var_select;
% ann: val_select;
% solve :: int_search(shoot, var_select, val_select, complete) minimize cost;

constraint

  global_cardinality_low_up(shoot, [1,2,3,4,5], [0,0,0,0,0], [5,5,5,5,5]) % :: domain

  /\ % symmetry breaking suggested by Van Hentenryck's paper cited above
  forall(s in 2..maxScene) (
    shoot[s] <= 1 + max([shoot[p] | p in 1..s-1])
  )

  /\
  forall(s in 1..maxScene, a in appears[s], d in 1..maxDay) (
    isShot[s,d] <= isPaid[a,d]
  )

  /\
  forall(s in 1..maxScene) (
     sum(d in 1..maxDay) (isShot[s,d]) = 1
  )

  /\
  forall(d in 1..maxDay) (
     sum(s in 1..maxScene) (isShot[s,d]) <= 5
  )

  /\
  forall(s in 1..maxScene) (
    isShot[s,shoot[s]] = 1
  )

  % symmetry breaking
  /\
  shoot[1] = 1 % Scene 1 is shoot at day 1

;

output [
  "cost: " ++ show(cost) ++ "\n" ++
  show(shoot) ++ "\n"
]
% ++
% [ 
%   if fix(shoot[s]) = d then 
%     "Day " ++ show(d) ++ " scene: " ++ show(s) ++ " actors: " ++ show([ActorS[a] | a in 1..numActors where a in appears[s]]) ++ " cost: " ++ show(sum([pay[a] | a in 1..numActors where a in appears[s]])) ++ "\n"
%   else 
%     ""
%   endif

%   | d in 1..maxDay, s in 1..maxScene
% ]
% ++
% [
%   "cost: " ++ show(cost) ++ "\n"
% ]
;


%
% data
%
maxDay = 5;
maxScene = 19;
int: Patt = 1;
int: Casta = 2;
int: Scolaro = 3;
int: Murphy = 4;
int: Brown = 5;
int: Hacket = 6;
int: Anderson = 7;
int: McDougal = 8;
int: Mercer = 9;
int: Spring = 10;
int: Thompson = 11;

numActors = 11;

ActorS = ["Patt", "Casta", "Scolaro", "Murphy", "Brown", "Hacket", "Anderson",
"McDougal", "Mercer", "Spring", "Thompson"];

Actor = [
Patt, Casta, Scolaro, Murphy, Brown, Hacket, Anderson,
McDougal, Mercer, Spring, Thompson
];

% Van Hentenryck's payment
pay = [26481, 25043, 30310 , 4085 , 7562, 9381 , 8770, 5788, 7423 , 3303, 9593];
% Dennis E. Shasha's payments
% pay = [264810, 250430, 303100 , 40850 , 75620, 93810 , 87700, 57880, 74230 , 33030, 95930];

appears = 
[
{Hacket}, % 1
{Patt,Hacket,Brown,Murphy}, % 2
{McDougal,Scolaro,Mercer,Brown}, % 3
{Casta,Mercer}, % 4
{Mercer,Anderson,Patt,McDougal,Spring}, % 5
{Thompson,McDougal,Anderson,Scolaro,Spring}, % 6
{Casta,Patt}, % 7
{Mercer,Murphy}, % 8
{Casta,McDougal,Mercer,Scolaro,Thompson}, % 9
{Casta,McDougal,Scolaro,Patt}, % 10
{Patt}, % 11
{Hacket,Thompson,McDougal,Murphy,Brown}, % 12
{Hacket,Murphy,Casta,Patt}, % 13
{Anderson,Scolaro}, % 14
{Thompson,Murphy,McDougal,Patt}, % 15
{Scolaro,McDougal,Casta,Mercer}, % 16
{Scolaro,Patt,Brown}, % 17
{Scolaro,McDougal,Hacket,Thompson}, % 18
{Casta} % 19
];
