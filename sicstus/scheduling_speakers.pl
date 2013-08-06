/*

  Scheduling speakers in SICStus Prolog.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/scheduling_speakers.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

/*
  Answer:

     6,3,5,2,4,1
     6,4,5,2,3,1
*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


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
        write(L),nl,nl,
        fd_statistics.


schedule_speakers(N,Available,Xs) :-

        % the alotted speaker slot
        length(Xs, N),
        domain(Xs,1,N),

        all_different(Xs),
        ( foreach(X, Xs),
          foreach(A,Available) do
              member(X,A)
        ),

        labeling([], Xs).
