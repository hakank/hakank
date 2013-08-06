/*
 
  Monks and doors problem in MiniZinc.
 
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


  Also, see the MiniZinc model: http://www.hakank.org/minizinc/monks_and_doors.mzn

  This  model was created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic).

go :-
        Doors = [A,B,C,D],
        Doors :: 0..1,
        Monks = [M1,M2,M3,M4,M5,M6,M7,M8],
        Monks :: 0..1,

        % Monk 1: Door A is the exit.
        M1 #= A, % for ic
        
        %  Monk 2: At least one of the doors B and C is the exit.
        M2 #= (B + C #>= 1),
        
        %  Monk 3: Monk 1 and Monk 2 are telling the truth.
        M3 #= (M1 and M2),
        
        %  Monk 4: Doors A and B are both exits.
        M4 #= (A and B) ,
        
        %  Monk 5: Doors A and C are both exits.
        M5 #= (A and C),
        
        %  Monk 6: Either Monk 4 or Monk 5 is telling the truth.
        M6 #= (M4 or M5),
        
        %  Monk 7: If Monk 3 is telling the truth, so is Monk 6.
        M7 #= (M3 => M6),
        
        %  Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
        M8 #= ((M7 and M8) => (M1)),
        
        % Exactly one door is an exit.
        (A + B + C + D) #= 1,
        
        append(Doors, Monks, FD),
        labeling(FD),

        writeln(exit_doors:Doors),
        writeln(truth_telling_monks:Monks).
