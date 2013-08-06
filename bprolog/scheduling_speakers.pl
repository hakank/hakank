/*

  Scheduling speakers in B-Prolog.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

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
        foreach(S in L, writeln(S)),
        nl.


schedule_speakers(N,Available,Xs) :-

        % the alotted speaker slot
        length(Xs, N),
        Xs :: 1..N,

        all_different(Xs),

        foreach((X,A) in (Xs,Available),  member(X,A) ),

        labeling([], Xs).
