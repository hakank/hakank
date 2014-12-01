% 
% Thick as Thieves in MiniZinc.
% 
% From http://l4f.cecs.anu.edu.au/puzzles/expert/thick-as-thieves
% """
% Following a robbery at Sparkles the Jeweller's, Inspector Korner of the Yard 
% interviewed six of the usual suspects. He knew that the getaway car had been 
% barely big enough to hold two, so he reckoned that at least four of them 
% were innocent – but which ones? He also supposed that the innocent ones 
% would tell the truth, while the guilty one or ones would lie. 
% What they actually said was:
%
%     ARTIE: "It wasn’t me."
%     BILL: "Crackitt was in it up to his neck."
%     CRACKITT: "No I wasn’t."
%     DODGY: "If Crackitt did it, Bill did it with him."
%     EDGY: "Nobody did it alone."
%     FINGERS: "That’s right: it was Artie and Dodgy together."
%
% If the good inspector’s suppositions were correct, who dunnit?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;

array[1..n] of string: names = ["Artie","Bill","Crackitt","Dodgy","Edgy","Fingers"];

% decision variables

% who dunnit
var 0..1: artie;
var 0..1: bill;
var 0..1: crackitt;
var 0..1: dodgy;
var 0..1: edgy;
var 0..1: fingers;
array[1..n] of var 0..1: x = [artie,bill,crackitt,dodgy,edgy,fingers];

% who told the truth
var 0..1: artie_t;
var 0..1: bill_t;
var 0..1: crackitt_t;
var 0..1: dodgy_t;
var 0..1: edgy_t;
var 0..1: fingers_t;
array[1..n] of var 0..1: x_t = [artie_t,bill_t,crackitt_t,dodgy_t,edgy_t,fingers_t];


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   % ARTIE: "It wasn’t me."
   (artie_t = 1 <->  artie = 0)
   
   % BILL: "Crackitt was in it up to his neck."
   /\ (bill_t = 1 <-> crackitt = 1)

   % CRACKITT: "No I wasn’t."
   /\ (crackitt_t = 1 <-> crackitt = 0)

   % DODGY: "If Crackitt did it, Bill did it with him."
   /\ (dodgy_t = 1 <-> ( crackitt = 1 -> bill = 1))

   % EDGY: "Nobody did it alone."
   /\ (edgy_t = 1 <-> sum(x) > 1)

   % FINGERS: "That’s right: it was Artie and Dodgy together."
   /\ (fingers_t = 1 <-> (artie = 1 /\ dodgy = 1))

   /\ sum(x_t) >= 4 % at least 4 innocent, i.e. told the truth
   /\ sum(x) <= 2   % at least 2 guilty
   /\
   forall(i in 1..n) (
       % if telling the truth <-> didn't do it
       x_t[i] = 1 <-> x[i] = 0
   )

;

output [
   "x: ", show(x), "\n",
   "x_t: ", show(x_t), "\n",
   "Who did it: ", show(join(", ", [names[i] | i in 1..n where fix(x[i])= 1])), "\n"
];

