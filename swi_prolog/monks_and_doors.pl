/*

  Monks and doors problem in SWI Prolog

  From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
  """
  There is a room with four doors and eight monks. One or more of
  the doors may be exit. Each monk is either telling a lie or the truth.
 
  The monks make the following statements:
  Monk 1: Door A is the exit.
  Monk 2: At least one of the doors B and C is the exit.
  Monk 3: Monk 1 and Monk 2 are telling the truth.
  Monk 4: Doors A and B are both exits.
  Monk 5: Doors A and B are both exits.
  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
 
  Which door is an exit no matter who is a liar and who is telling the
  truth.
  """
 
  Answer: Door A is an exit.
          And monks 1, 7, and 8 are telling the truth.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   Doors = [A,B,C,D],
   Doors ins 0..1,
   Monks = [M1,M2,M3,M4,M5,M6,M7,M8],
   Monks ins 0..1,

   % Monk 1: Door A is the exit.
   M1 #= A, 
   
   %  Monk 2: At least one of the doors B and C is the exit.
   M2 #= 1 #<==> (B + C #>= 1),
   
   %  Monk 3: Monk 1 and Monk 2 are telling the truth.
   M3 #= 1 #<==> (M1 #/\ M2),
   
   %  Monk 4: Doors A and B are both exits.
   M4 #= 1 #<==> (A #/\ B) ,
   
   %  Monk 5: Doors A and C are both exits.
   M5 #= 1 #<==> (A #/\ C),
   
   %  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
   M6 #= 1 #<==> (M4 #\/ M5),
   
   %  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
   M7 #= 1 #<==> (M3 #==> M6),
   
   %  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
   M8 #= 1 #<==> ((M7 #/\ M8) #==> (M1)),
   
   % Exactly one door is an exit.
   A + B + C + D #= 1,
   
   flatten([Doors,Monks],Vars),
   labeling([], Vars),

   writeln(exit_doors=Doors),
   DoorsS = ["A","B","C","D"],
   findall(ED,(
              between(1,4,I),
              element(I,Doors,1),
              nth1(I,DoorsS,ED)
             ),
           ExitDoors),
   writeln(exit_doors=ExitDoors),
   writeln(truth_telling_monks=Monks),
   findall(I,(
              between(1,8,I),
              element(I,Monks,1)
             ),
           TruthTellingMonks),
   writeln(truth_telling_monks=TruthTellingMonks),
   nl.
