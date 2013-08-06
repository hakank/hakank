/*

  Scheduling speakers in ECLiPSe.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/scheduling_speakers.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/scheduling_speakers.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).

go :-        
        N = 6, % number of speakers

        % the available slots 
        Available = [
                                      % Reasoning:             
                        [3,4,5,6],    % 2) the only one with 6 after speaker F -> 1
                        [3,4],        % 5) 3 or 4
                        [2,3,4,5],    % 3) only with 5 after F -> 1 and A -> 6
                        [2,3,4],      % 4) only with 2 after C -> 5 and F -> 1 
                        [3,4],        % 5) 3 or 4
                        [1,2,3,4,5,6] % 1) the only with 1
                    ],
        
        findall(X,schedule_speakers(N,Available,X),L),
        write(L),nl,nl.


schedule_speakers(N,Available,Xs) :-

        % the alotted speaker slot
        length(Xs, N),
        Xs :: 1..N,

        alldifferent(Xs),
        ( foreach(X, Xs),
          foreach(A,Available) do
              member(X,A)
        ),

        labeling(Xs).
